from pathlib import Path

from scipy.interpolate import interpolate

from tadpole.models.tadpole_model import TadpoleModel
import pandas as pd
import numpy as np


class ECMEB(TadpoleModel):
    def preprocess(self, train_df: pd.DataFrame):
        train_df = train_df.copy()
        if 'Diagnosis' not in train_df.columns:
            train_df = train_df.replace({'DXCHANGE': {4: 2, 5: 3, 6: 3, 7: 1, 8: 2, 9: 1}})
            train_df = train_df.rename(columns={"DXCHANGE": "Diagnosis"})

        train_df['AGE'] += train_df['Month_bl'] / 12.

        h = list(train_df)
        train_df: pd.DataFrame = train_df.drop(
            h[1:8] + [h[9]] + h[14:17] + h[45:47] + h[53:73] + h[74:486] + h[832:838] + h[1172:1174] + h[1657:1667] + h[1895:1902] + h[1905:],
            axis=1
        )

        h = list(train_df)

        print('Forcing Numeric Values')
        for i in range(5, len(h)):
            if train_df[h[i]].dtype != 'float64':
                train_df[h[i]] = pd.to_numeric(train_df[h[i]], errors='coerce')

        train_df = train_df.sort_values(by=['RID', 'AGE'])

        train_df = train_df.drop(['EXAMDATE', 'AGE', 'PTGENDER', 'PTEDUCAT', 'APOE4'], axis=1)

        if 'Ventricles_ICV' not in train_df.columns:
            train_df["Ventricles_ICV"] = train_df["Ventricles"].values / train_df["ICV_bl"].values

        return train_df


    def set_futures(self, train_df):
        # Get future value from each row's next row, e.g. shift the column one up
        for predictor in ["Diagnosis", "ADAS13", 'Ventricles_ICV']:
            train_df["Future_" + predictor] = train_df[predictor].shift(-1)

        # Drop each last row per patient
        train_df = train_df.drop(train_df.groupby('RID').tail(1).index.values)
        return train_df

    @staticmethod
    def fill_nans_by_older_values(train_df):
        # Fill nans in feature matrix by older values
        train_df.fillna(method='ffill')
        for rid in train_df['RID'].unique():
            idx_rid = train_df.index[train_df["RID"] == rid]
            train_df.iloc[idx_rid] = train_df.iloc[idx_rid].fillna(method='ffill').fillna(method='bfill')

        return train_df

        #     idx_rid = train_df.index[train_df["RID"] == rid]
        #     for column_index in range(0, len(train_df.columns)):
        #         selection = train_df.iloc[idx_rid, [column_index]]
        #         try:
        #             train_df.iloc[idx_rid, column_index] = selection.reset_index().interpolate(
        #                     method='nearest',
        #                     kind='zero',
        #                     fill_value='extrapolate',
        #                 ).set_index('index')
        #             pass
        #         except Exception as e:
        #             pass  # only nans in column




    def train(self, train_set_path):
        train_df = pd.read_csv(train_set_path)
        train_df = self.preprocess(train_df)
        train_df = self.set_futures(train_df)

        selected_features = pd.read_csv(Path(__file__).parent / 'ECMEB_features.csv')['feature'].values.tolist()
        selected_features = selected_features[0:200]
        selected_features += ['RID', 'Future_Diagnosis', 'Future_ADAS13', 'Future_Ventricles_ICV', 'Diagnosis', 'Ventricles_ICV']
        train_df = train_df.copy()[selected_features]

        train_df = self.fill_nans_by_older_values(train_df)

        train_df = train_df.drop(['RID'], axis=1)

        # Fill nans in feature matrix & scale
        Dtrainmat = train_df.values  # Method .as_matrix will be removed in a future version. Use .values instead.
        m = []
        s = []
        for i in range(Dtrainmat.shape[1]):
            m.append(np.nanmean(Dtrainmat[:, i]))
            s.append(np.nanstd(Dtrainmat[:, i]))
            Dtrainmat[np.isnan(Dtrainmat[:, i]), i] = m[i]

            # scale
            Dtrainmat[:, i] = (Dtrainmat[:, i] - m[i]) / s[i]

        train_df_diagnosis = train_df[train_df['Future_Diagnosis'].notnull()]
        train_df_adas = train_df[train_df['Future_ADAS13'].notnull()]
        train_df_ventricles = train_df[train_df['Future_Ventricles_ICV'].notnull()]

        pass

        pass

    def predict(self, test_set_path, datetime):
        pass


if __name__ == '__main__':
    ecmeb = ECMEB()
    ecmeb.train(Path('../../data/train_short.csv'))
