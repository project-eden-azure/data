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
	necessary = ['lat', 'lng', 'hour', 'weekday']
	for parameter in necessary:
		if not request.args.get(parameter):
			return "Missing inputs: <em>{}</em> not found.".format(parameter), 500

	lat = request.args.get('lat')
	lng = request.args.get('lng')
	hour = request.args.get('hour')
	weekday = request.args.get('weekday')

	# Make a prediction
	# TODO

	# Return the predicted ETA
	return "Successfully received inputs: {}, {}, {}, {}".format(lat, lng, hour, weekday)

if __name__ == "__main__":
	app.debug = False
	app.run(host = "0.0.0.0", port = 80, threaded = True)
	#http_server = HTTPServer(WSGIContainer(app))
	#http_server.listen(80)
	#IOLoop.instance().start()