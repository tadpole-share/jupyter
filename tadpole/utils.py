import random
import numpy as np

def test_train_split(df, test_fraction=0.1):
    # remove all rows without 'Ventricles"
    # df_clean = df[df["Ventricles"].notnull()]
    patient_ids = set(df["RID"].unique().tolist())

    # split 90/10 train/test
    train_patient_ids = set(random.sample(patient_ids, int(len(patient_ids) * test_fraction)))
    test_patient_ids = patient_ids - train_patient_ids
    train_df = df[df['RID'].isin(train_patient_ids)]

    # combined test & ground truth
    test_ground_truth_df = df[df['RID'].isin(test_patient_ids)]

    # drop all patients with only one record
    test_ground_truth_df = test_ground_truth_df.groupby('RID').filter(lambda x: len(x) > 1)

    # ground truth is last row per patient
    ground_truth_df = test_ground_truth_df.groupby('RID').tail(1)

    # test set is the rest of the rows
    test_df = test_ground_truth_df.drop(ground_truth_df.index)

    assert len(ground_truth_df.groupby('RID')) == len(test_df.groupby('RID'))

    return train_df.reset_index(drop=True),\
           test_df.reset_index(drop=True),\
           ground_truth_df.reset_index(drop=True)


def ventricles_conf_interv(Ventricles_Col, ICV_Col, margin=1000):
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


def predefined_status_prediction(status):
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
