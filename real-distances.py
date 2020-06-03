import numpy as np
import pandas as pd
import pickle
import os
from datetime import datetime
import pyarrow.parquet as pq
import multiprocessing as mp
import collections
import itertools
import math

# Find the true distances travelled along roads by trip ID using MapReduce.
# (This is the road-by-road Haversine distance rather than the Euclidean distance.)

class SimpleMapReduce(object):  
	def __init__(self, map_func, reduce_func, num_workers=None):
		"""
		map_func

		  Function to map inputs to intermediate data. Takes as
		  argument one input value and returns a tuple with the key
		  and a value to be reduced.
		
		reduce_func

		  Function to reduce partitioned version of intermediate data
		  to final output. Takes as argument a key as produced by
		  map_func and a sequence of the values associated with that
		  key.
		 
		num_workers

		  The number of workers to create in the pool. Defaults to the
		  number of CPUs available on the current host.
		"""
		self.map_func = map_func
		self.reduce_func = reduce_func
		self.pool = mp.Pool(num_workers)
	
	def partition(self, mapped_values):
		"""Organize the mapped values by their key.
		Returns an unsorted sequence of tuples with a key and a sequence of values.
		"""
		partitioned_data = collections.defaultdict(list)
		for key, value in mapped_values:
			partitioned_data[key].append(value)
		return partitioned_data.items()
	
	def __call__(self, inputs, chunksize=1):
		"""Process the inputs through the map and reduce functions given.
		
		inputs
		  An iterable containing the input data to be processed.
		
		chunksize=1
		  The portion of the input data to hand to each worker.  This
		  can be used to tune performance during the mapping phase.
		"""
		print('Starting mapping')
		map_responses = self.pool.map(self.map_func, inputs, chunksize=chunksize)
		print('Finished mapping')
		partitioned_data = self.partition(itertools.chain(*map_responses))
		print('Finished partitioning')
		reduced_values = self.pool.map(self.reduce_func, partitioned_data)
		print('Finished reducing')
		return reduced_values

# Haversine distance - convert latitude/longitude to kilometres
def measure(lat1, lon1, lat2, lon2):
	lon1, lat1, lon2, lat2 = map(math.radians, [lon1, lat1, lon2, lat2])
	R = 6378.137 # Radius of earth in kilometres
	dLat = lat2 * math.pi / 180 - lat1 * math.pi / 180
	dLon = lon2 * math.pi / 180 - lon1 * math.pi / 180
	a = math.sin(dLat/2) * math.sin(dLat/2) + math.cos(lat1 * math.pi / 180) * math.cos(lat2 * math.pi / 180) * math.sin(dLon/2) * math.sin(dLon/2)
	c = 2 * math.atan2(math.sqrt(a), math.sqrt(1-a))
	d = R * c
	return d

# Reads parquet data from a file and maps by trip id
def map_function(filename):
	print(mp.current_process().name, 'reading', filename)

	output = []
	df = pq.read_table(filename).to_pandas()
	for index, row in df.iterrows():
		if row['trj_id'] is None or row['pingtimestamp'] is None or row['rawlat'] is None or row['rawlng'] is None:
			continue
		# Extract distance travelled
		output.append((row['trj_id'], (row['pingtimestamp'], row['rawlat'], row['rawlng'])))

	return output

# Reduces sets of data into a single distance in kilometres
def reduce_function(bundle):
	key, items = bundle
	# Sort by timestamp
	sorted_items = sorted(items, key=lambda s: s[0])
	total_distance  = 0
	for index, item in enumerate(sorted_items):
		if index == 0:
			continue
		distance = measure(items[index][1], items[index][2], items[index-1][1], items[index-1][2])
		total_distance += distance

	return (key, total_distance)

# Run the distance 
if __name__ == '__main__':
	import glob

	input_files = glob.glob('part-00000-8bbff892-97d2-4011-9961-703e38972569.c000.snappy.parquet')
	mapper = SimpleMapReduce(map_function, reduce_function)
	distances = mapper(input_files)

	df = pd.DataFrame(distances, columns=['ID', 'km'])

	df.to_csv("distances_km.csv", index=False)