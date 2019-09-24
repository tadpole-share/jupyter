import pandas

from pathlib import Path

d1d2_path = Path('../data/TADPOLE_D1_D2.csv')
d4_path = Path('../data/TADPOLE_D4_corr.csv')
test_set_path = Path('../data/tadpole_test_set.csv')


d1d2_df = pandas.read_csv(d1d2_path)
d4_df = pandas.read_csv(d4_path)

test_df = d1d2_df[
    d1d2_df['RID'].isin(d4_df['RID'].unique())
]

test_df.to_csv(test_set_path, index=False)
