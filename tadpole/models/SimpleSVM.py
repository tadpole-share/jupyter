import pandas as pd
import numpy as np
from datetime import datetime
from dateutil.relativedelta import relativedelta
from sklearn import svm
from sklearn.preprocessing import StandardScaler
from sklearn.pipeline import Pipeline

from tqdm import tqdm_notebook as tqdm


class SimpleSVM:
    def __init__(self):
        self.diagnosis_model = Pipeline([
            ('scaler', StandardScaler()),
            ('classifier', svm.SVC(kernel="linear", probability=True)),
        ])
        self.adas_model = Pipeline([
            ('scaler', StandardScaler()),
            ('classifier', svm.SVR(kernel="linear")),
        ])
        self.ventricles_model = Pipeline([
            ('scaler', StandardScaler()),
            ('classifier', svm.SVR(kernel="linear")),
        ])

    def train_model(self, model, train_df, X_train, var_name):
        # remove rows with NaN future values
        Y_train_var = train_df[var_name]
        not_nans = np.logical_not(np.isnan(Y_train_var))
        X_train_var = X_train[not_nans]
        Y_train_var = Y_train_var[not_nans]
        model.fit(X_train_var, Y_train_var)

    def pre_process(self, train_df):
        train_df = train_df.copy()
        if 'Diagnosis' not in train_df.columns:
            train_df.replace({'DXCHANGE': {4: 2, 5: 3, 6: 3, 7: 1, 8: 2, 9: 1}})
            train_df = train_df.rename(columns={"DXCHANGE": "Diagnosis"})

        # Sort the dataframe based on age for each subject
        train_df = train_df.sort_values(by=['RID', 'Years_bl'])

        if 'Ventricles_ICV' not in train_df.columns:
            train_df["Ventricles_ICV"] = train_df["Ventricles"].values / train_df["ICV_bl"].values

        # Select features
        train_df = train_df[
            ["RID", "Diagnosis", "ADAS13", "Ventricles_ICV", "Ventricles", "ICV_bl"]
        ]

        # Force values to numeric
        train_df = train_df.astype("float64", errors='ignore')

        return train_df

    def set_futures(self, train_df):
        # Get future value from each row's next row, e.g. shift the column one up
        for predictor in ["Diagnosis", "ADAS13", 'Ventricles_ICV']:
            train_df["Future_" + predictor] = train_df[predictor].shift(-1)

        # Drop each last row per patient
        train_df = train_df.drop(train_df.groupby('RID').tail(1).index.values)
        return train_df

    def train(self, train_df: pd.DataFrame):
        train_df = self.pre_process(train_df)
        train_df = self.set_futures(train_df)

        # Select columns for training
        X_train = train_df[["Diagnosis", "ADAS13", "Ventricles_ICV"]]

        # fill NaNs with mean
        X_train = X_train.fillna(X_train.mean())

        self.train_model(self.diagnosis_model, train_df, X_train, "Future_Diagnosis")
        self.train_model(self.adas_model, train_df, X_train, "Future_ADAS13")
        self.train_model(self.ventricles_model, train_df, X_train, "Future_Ventricles_ICV")

    def predict(self, predict_df: pd.DataFrame, datetime):
        # Do a single prediction for a single patient.
        predict_df = predict_df.sort_values(by=['EXAMDATE'])
        predict_df_preprocessed = self.pre_process(predict_df)

        # get the final row (last known value that is not NaN for each variable)
        final_row = [
            predict_df_preprocessed['Diagnosis'].dropna().iloc[-1],
            predict_df_preprocessed['ADAS13'].dropna().iloc[-1],
            predict_df_preprocessed['Ventricles_ICV'].dropna().iloc[-1]
        ]

        # TODO: The evaluation code expects as input a list of 5*12 (5 years of
        # monthly) predictions. How to incorporate this into our code?
        diagnosis = self.diagnosis_model.predict([final_row])[0]
        adas13 = self.adas_model.predict([final_row])[0]
        ventricles_icv = self.ventricles_model.predict([final_row])[0]

        # Algorithms have to provide the probabilities of each of the
        # diagnoses.
        # In the most simple case, if an algorithm doesn't provide
        # probabilities, it is possible to set two of the probabilities to zero
        # and the others to one.
        # Here we use predefined probabilities.
        CNp, MCIp, ADp = self.predef_status_prediction(diagnosis)

        # Algorithms also have to provide the confidence intervals for ADAS13
        # and Ventricles_ICV.
        # Here we use some simple approximations.
        conf_v = self.ventr_conf_interv(predict_df_preprocessed['Ventricles'],
                                        predict_df_preprocessed['ICV_bl'])

        return {
            'Diagnosis': diagnosis,
            'ADAS13': adas13,
            'Ventricles_ICV': ventricles_icv,
            'Forecast Date': datetime.strftime('%Y-%m'),
            'RID': predict_df['RID'].dropna().iloc[-1],
            'CN relative probability': CNp,
            'MCI relative probability': MCIp,
            'AD relative probability': ADp,
            'ADAS13 50% CI lower': max([0, adas13-1]),
            'ADAS13 50% CI upper': adas13+1,
            'Ventricles_ICV 50% CI lower': ventricles_icv - conf_v,
            'Ventricles_ICV 50% CI upper': ventricles_icv + conf_v
        }


    def predict_all_months(self, predict_df: pd.DataFrame, num=5*12):
        predictions = []

        for _rid, patient_df in tqdm(predict_df.groupby('RID')):
            # What is the start date of the predictions? One month after the
            # last data point?
            # TODO: fix case when the last row of predict_df contains nans in
            # places where we don't want them.
            patient_df = patient_df.sort_values(by=['EXAMDATE'])
            start = patient_df['EXAMDATE'].iloc[-1]
            if isinstance(start, str):
                start = datetime.strptime(start, '%Y-%m-%d')
            pred_date = start + relativedelta(months=1)

            for _ in range(num):
                prediction = self.predict(patient_df, pred_date)
                predictions.append(prediction)

                # Algorithms are free to use the predicted data for future
                # predictions. Here we just use the test data.

                pred_date = pred_date + relativedelta(months=1)

        return pd.DataFrame(predictions)

    def ventr_conf_interv(self, Ventricles_Col, ICV_Col, margin=1000):
        # code adapted from: https://github.com/tadpole-share/TADPOLE-eval/blob/c8d4e241bc143b858d9b8237aab92417d3e871e2/evaluation/TADPOLE_SimpleForecastExampleLeaderboard.py#L164
        # Missing data = typical volume +/- broad interval = 25000 +/- 20000

        Ventricles_ICV_Col = Ventricles_Col/ICV_Col

        # Convert to Ventricles/ICV via linear regression
        nm = np.all(np.stack([Ventricles_Col>0,ICV_Col>0]),0) # not missing: Ventricles and ICV
        x = Ventricles_Col[nm]
        y = Ventricles_ICV_Col[nm]
        lm = np.polyfit(x,y,1)
        p = np.poly1d(lm)

        return np.abs(p(margin) - p(-margin))/2


    def predef_status_prediction(self, status):
        # code adapted from: https://github.com/tadpole-share/TADPOLE-eval/blob/c8d4e241bc143b858d9b8237aab92417d3e871e2/evaluation/TADPOLE_SimpleForecastExampleLeaderboard.py#L235
        #* Clinical status forecast: predefined likelihoods per current status

        # ? CN or NL?
        if status == 1:
            CNp, MCIp, ADp = (0.75, 0.15, 0.1)
        # MCI
        elif status == 2:
            CNp, MCIp, ADp = (0.1, 0.5, 0.4)
        # ? Dementia or AD?
        elif status == 3:
            CNp, MCIp, ADp = (0.1, 0.1, 0.8)
        else:
            CNp, MCIp, ADp = (0.33, 0.33, 0.34)

        return CNp, MCIp, ADp
