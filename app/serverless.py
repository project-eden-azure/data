import requests
import math
from math import radians, cos, sin, asin, sqrt
import os
import json
import time
import rpy2.robjects as robjects

# Returns distance predicted by Azure Maps in kilometres.
def azure_distance(lat1, lng1, lat2, lng2):
	url = "https://atlas.microsoft.com/route/directions/json?subscription-key=k27huD7zz1pT1QccsQyhNCayMg5sjUCLhqdWQcmUFRw"
	query = "&api-version=1.0&query="
	coordinates = str(lat1) + "," + str(lng1) + ":" + str(lat2) + "," + str(lng2)
	params = "&travelMode=car"

	full_url = url + query + coordinates + params

	r = requests.get(url = full_url)
	data = r.json()

	distance = data["routes"][0]["summary"]["lengthInMeters"]

	return float(distance) / 1000


def osrm_distance(lat1, lng1, lat2, lng2, local=False):
	if local:
		address = "http://localhost:5000"
	else:
		address = "http://router.project-osrm.org"

	# These are lng then lat - care.
	coordinates = str(lng1) + "," + str(lat1) + ";" + str(lng2) + "," + str(lat2)
	query = address + "/route/v1/driving/" + coordinates + "?overview=full"

	r = requests.get(url = query)
	data = r.json()

	if data["code"] != "Ok":
		# Invalid
		return None

	distance = data["routes"][0]["distance"]

	return float(distance) / 1000

def haversine_distance(lat1, lng1, lat2, lng2):
    """
    Calculate the great circle distance between two points 
    on the earth (specified in decimal degrees)
    Returns the distance in kilometres
    """
    # Convert decimal degrees to radians 
    lng1, lat1, lng2, lat2 = map(radians, map(float, [lng1, lat1, lng2, lat2]))

    # Haversine formula 
    dlon = lng2 - lng1 
    dlat = lat2 - lat1 
    a = sin(dlat/2)**2 + cos(lat1) * cos(lat2) * sin(dlon/2)**2
    c = 2 * asin(sqrt(a)) 
    r = 6378.137 # Radius of earth in kilometers. Use 3956 for miles
    return c * r

# Determines if an hour is a rush hour.
def get_rush_hour(hour):
	rush_hour = "No"
	if hour > 7 and hour < 10:
		rush_hour = "Morning"
	elif hour > 17 and hour < 20:
		rush_hour = "Night"
	elif hour > 10 and hour < 17:
		rush_hour = "Work"
	return rush_hour

def get_weekday(weekday):
	if weekday == 0:
		return "Mon"
	elif weekday == 1:
		return "Tue"
	elif weekday == 2:
		return "Wed"
	elif weekday == 3:
		return "Thu"
	elif weekday == 4:
		return "Fri"
	elif weekday == 5:
		return "Sat"
	elif weekday == 6:
		return "Sun"

	return None

# Called when the deployed service starts
def init():
	model_path = os.path.join(os.getenv('AZUREML_MODEL_DIR'), './model')

	robjects.r(" load( '" + model_path + "/impute_dist2.RData' ) ")
	robjects.r(" source( '" + model_path + "/impute_distances.R' ) ")
	robjects.r(" load( '" + model_path + "/impute_dist.RData' ) ")
	robjects.r(" load( '" + model_path + "/historical.RData' ) ")
	robjects.r(" source( '" + model_path + "/historical.R' ) ")
	robjects.r(" load( '" + model_path + "/impute_speed_model.RData' ) ")
	robjects.r(" source( '" + model_path + "/impute_speed.R' ) ")
	robjects.r(" load( '" + model_path + "/stack_model2.RData' ) ")
	robjects.r(" source( '" + model_path + "/stack_training.R' ) ")

# Handle requests to the service
def run(data):
	try:
		data = json.loads(data)

		lat1 =data['originLat']
		lng1 = data['originLong']
		lat2 = data['destinationLat']
		lng2 = data['destinationLong']
		hour = data['hour']
		weekday = data['weekday']

		hour = int(hour)
		weekday = int(weekday)

		# Make a prediction
		# Get the Azure Maps distance.
		# TODO (non-urgent) - use departAt parameter in Azure maps (which takes a date-time in the format 1996-12-19T16:39:57-08:00)
		azure_distance = azure_distance(lat1, lng1, lat2, lng2)
		# Get OSRM distance.
		# TODO - Change local to True when we have a local version of OSRM running. Or leave as False for stability.
		osrm_distance = osrm_distance(lat1, lng1, lat2, lng2, local=False)
		# Haversine / crow-flies distance
		crow_distance = haversine_distance(lat1, lng1, lat2, lng2)

		# Whether it's a rush hour as a factor
		rush_hour = get_rush_hour(hour)
		# Weekday as a factor
		f_weekday = get_weekday(weekday)

		prediction = predict(lat1, lng1, lat2, lng2, hour, f_weekday, rush_hour, azure_distance, osrm_distance, crow_distance)
		return {"eta": prediction}
	except Exception as e:
		error = str(e)
		return error

# Calls model
def predict(lat1, lng1, lat2, lng2, hour, f_weekday, rush_hour, azure_distance, osrm_distance, crow_distance):
	robjects.r(" OOB_data = data.frame(lat1, lng1, lat2, lng2, hour, 'f_weekday', 'rush_hour', azure_distance, osrm_distance, crow_distance) ")
	robjects.r(" OOB_imputed_path_dist2 = dist_impute_model_predict( impute_path_dist2, OOB_data ) ")
	robjects.r(" OOB_data[ , path_dist2_impute := OOB_imputed_path_dist2 ] ")
	robjects.r(" OOB_impute_path_dist = dist_impute_model_predict( impute_path_dist, OOB_data ) ")
	robjects.r(" OOB_data[ , path_dist_impute := OOB_impute_path_dist ] ")
	robjects.r(" join_historical_data( OOB_data, historical_data ) ")
	robjects.r(" OOB_imputed_speed = speed_impute_model_predict( impute_speed, OOB_data ) ")
	robjects.r(" OOB_data[ , mean_speed_impute := OOB_imputed_speed ] ")
	robjects.r(" final_predictions = stack_predictions( stacking_model, OOB_data ) ")
	predicted_time = robjects.r("final_predictions[1,1]")
	return predicted_time