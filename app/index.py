from flask import Flask, render_template, url_for, session, request, g
import sqlite3
import os
from tornado.wsgi import WSGIContainer
from tornado.httpserver import HTTPServer
from tornado.ioloop import IOLoop

# Define the application.
app = Flask(__name__)
app.debug = False

# Random secret key, so session cookies cannot be modified to gain access to other sessions even by the administrators.
app.secret_key = os.urandom(24)

# App routing.
@app.route('/')
def index_page():
    return render_template('index.html', upload = True)

@app.route('/query', methods = ['GET'])
def query_page():
	# Check all parameters are filled in
	necessary = ['originLat', 'originLong', 'destinationLat', 'destinationLong', 'hour', 'weekday']
	for parameter in necessary:
		if not request.args.get(parameter):
			return "Missing inputs: <em>{}</em> not found.".format(parameter), 400

	lat1 = request.args.get('originLat')
	lng1 = request.args.get('originLong')
	lat2 = request.args.get('destinationLat')
	lng2 = request.args.get('destinationLong')
	hour = request.args.get('hour')
	weekday = request.args.get('weekday')

	# Validate data
	# TODO
	try:
		hour = int(hour)
		weekday = int(weekday)
	except:
		return "Invalid hour or weekday - should be numbers between 0-23 and 1-7.", 422
	if hour < 0 or hour > 23:
		return "Invalid hour: {}".format(hour), 422
	if weekday < 1 or weekday > 7:
		return "Invalid weekday: {}".format(weekday), 422

	# Make a prediction
	# TODO

	# Return the predicted ETA
	return "Successfully received inputs: {}, {}, {}, {}, {}, {}".format(lat1, lng1, lat2, lng2, hour, weekday)

if __name__ == "__main__":
	app.debug = False
	app.run(host = "0.0.0.0", port = 80, threaded = True)
	#http_server = HTTPServer(WSGIContainer(app))
	#http_server.listen(80)
	#IOLoop.instance().start()