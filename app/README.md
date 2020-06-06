To run, install the requirements using `pip3 install -r requirements.txt`.

Then run either:

`export FLASK_APP=index.py`

`python3 -m flask run`

Or:

`python index.py`

Go to `localhost/query?originLat=LAT&originLong=LNG&destinationLat=LAT&destinationLong=LNG&hour=HOUR&weekday=WEEKDAY` to generate predictions (when a model is done).