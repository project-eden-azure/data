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
		self.map_func = map_func
		self.reduce_func = reduce_func
		self.pool = mp.Pool(num_workers)
	
	def partition(self, mapped_values):
		partitioned_data = collections.defaultdict(list)
		for key, value in mapped_values:
			partitioned_data[key].append(value)
		return partitioned_data.items()
	
	def __call__(self, inputs, chunksize=1):
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
	dLat = lat2  - lat1
	dLon = lon2 - lon1

	a = math.sin(dLat/2) * math.sin(dLat/2) + math.cos(lat1 * math.pi / 180) * math.cos(lat2 * math.pi / 180) * math.sin(dLon/2) * math.sin(dLon/2)
	c = 2 * math.atan2(math.sqrt(a), math.sqrt(1-a))
	d = R * c
	return d

# Reads parquet data from a file and maps by trip id
def map_function(filename):
	print(mp.current_process().name, 'reading', filename)

	output = []
#	filename = input_files[0]
	df = pq.read_table(filename).to_pandas()
#	df = df.sort_values( by = "pingtimestamp")
    
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
    
	#sorted_items = output
    
    
	total_distance  = 0
	#distances = []
	for index, item in enumerate(sorted_items):
		if index == 0:
			continue
#		distance = measure(sorted_items[index][1][1], sorted_items[index][1][2], 
#                           sorted_items[index-1][1][1], sorted_items[index-1][1][2])
		distance = measure(items[index][1], items[index][2], items[index-1][1], items[index-1][2])

		total_distance += distance
		#distances.append(distance)
        
	return (key, total_distance)

# Run the distance 
if __name__ == '__main__':
	import glob

	input_files = glob.glob('mini.parquet')
	mapper = SimpleMapReduce(map_function, reduce_function)
	distances = mapper(input_files)

	df = pd.DataFrame(distances, columns=['ID', 'km'])

	df.to_csv("distances_km.csv", index=False)


