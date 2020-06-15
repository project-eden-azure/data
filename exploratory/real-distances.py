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
from math import radians, cos, sin, asin, sqrt

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
# lat/lng in degrees
def measure(lat1, lon1, lat2, lon2):	
	# Latitudes/longitudes may be negative so log may not work
	# dLat1 = log(lat2) + log(pi) - log(180)
	# dlat2 = log(lat1) + log(pi) - log(180)
	# dLay = exp( logsumexp( dLat1, dlat2 ) )
    """
    Calculate the great circle distance between two points 
    on the earth (specified in decimal degrees)
    """
    # convert decimal degrees to radians 
    lon1, lat1, lon2, lat2 = map(radians, [lon1, lat1, lon2, lat2])

    # haversine formula 
    dlon = lon2 - lon1 
    dlat = lat2 - lat1 
    a = sin(dlat/2)**2 + cos(lat1) * cos(lat2) * sin(dlon/2)**2
    c = 2 * asin(sqrt(a)) 
    r = 6378.137 # Radius of earth in kilometers. Use 3956 for miles
    return c * r

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

	total_time = sorted_items[-1][0] - sorted_items[0][0]
	average_speed = total_distance * 1000 / total_time
	return (key, total_distance, average_speed)

# Run the distance - MapReduce type - incorrect values but better than the correct values?
# if __name__ == '__main__':
# 	import glob

# 	input_files = glob.glob('part-00000-8bbff892-97d2-4011-9961-703e38972569.c000.snappy.parquet')
# 	mapper = SimpleMapReduce(map_function, reduce_function)
# 	distances = mapper(input_files)

# 	df = pd.DataFrame(distances, columns=['ID', 'km', 'average_speed'])

# 	df.to_csv("distances_km.csv", index=False)

# 
if __name__ == '__main__':
	import glob

	input_files = glob.glob('part-00000-8bbff892-97d2-4011-9961-703e38972569.c000.snappy.parquet') #'mini.parquet')#

	output = []
	for filename in input_files:
		data = pq.read_table(filename).to_pandas()

		trip_ids = set(data['trj_id'])
		# For each trip
		for trip_id in trip_ids:
			if trip_id is None or trip_id == '':
				continue
			# Filter by trip id and also sort by timestamp
			items = data[data['trj_id'] == trip_id]
			items = items.sort_values("pingtimestamp")
			items = items.drop_duplicates(subset="pingtimestamp")

			items['prevlat'] = items['rawlat'].shift(1)
			items['prevlng'] = items['rawlng'].shift(1)

			items['distance'] = items.apply(lambda row: measure(row['prevlat'], row['prevlng'], row['rawlat'], row['rawlng']), axis=1)

			items = items.fillna(0)
			total_distance = items['distance'].sum()

			# for i, row in items.iterrows():
			# 	if first:
			# 		first = False
			# 		last_i = i
			# 		continue
			# 	prevlat = 
			# 	distance = measure(row['rawlat'], row['rawlng'], prev_row['rawlat'], prev_row['rawlng'])
			# 	total_distance += distance
			# 	last_i = i

			output.append([trip_id, total_distance])


		df = pd.DataFrame(output, columns=['ID', 'km'])
		df.to_csv("distances_km_{}.csv".format(filename), index=False)
