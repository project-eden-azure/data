import numpy as np
import pandas as pd
import os
import pyarrow.parquet as pq

def read_parquet(filename):
    dataset = pq.read_table( filename ).to_pandas()
    return dataset
