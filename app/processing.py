import requests

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