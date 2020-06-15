import numpy as np
import pandas as pd
import pickle
import os
from datetime import datetime
import pyarrow.parquet as pq
from pyarrow import Table
import multiprocessing as mp
import collections
import itertools

# Creates a miniature dataset for testing purposes
# with the first 20000 entries.

filename =  'part-00000-8bbff892-97d2-4011-9961-703e38972569.c000.snappy.parquet'

df = pq.read_table(filename).to_pandas()

mini_df = df.head(20000)
table = Table.from_pandas(mini_df, nthreads=1)
pq.write_table(table, 'mini.parquet')