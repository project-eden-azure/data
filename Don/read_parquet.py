import numpy as np
import pandas as pd
import pickle
import os
from datetime import datetime
import pyarrow.parquet as pq
import matplotlib.pyplot as plt

pd.set_option( "display.max_columns", None )

filename = "part-00000-8bbff892-97d2-4011-9961-703e38972569.c000.snappy.parquet"
dataset = pq.read_table( filename ).to_pandas()
