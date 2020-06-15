import requests
import math
from math import radians, cos, sin, asin, sqrt

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