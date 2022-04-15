import pandas as pd
from sqlalchemy import create_engine
import os
import glob

con = 'postgresql://localhost/news'
con = create_engine(con)

path = '/Volumes/GoogleDrive/My Drive/Data/news_data/eventregistry'
extension = 'csv'
os.chdir(path)
result = [i for i in glob.glob('*.{}'.format(extension))]

for i in result:
    df = pd.read_csv(i, index_col= 0)

    df.columns = df.columns.str.strip().str.lower().str.replace(' ', '_').str.replace('(', '').str.replace(')', '')
    df = df[['body','date','isduplicate','lang','shares','source','time','title','uri','url']]

    df.to_sql(con=con, name='er_scrapes', if_exists='append', index=False)