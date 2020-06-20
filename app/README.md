To run, install the requirements using `pip3 install -r requirements.txt`.

Then run either:

`export FLASK_APP=index.py`

`python3 -m flask run`

Or follow the instructions in the main page's README file for deploying in production mode.

Upload predictions into a `model/` folder underneath this folder.

Go to `localhost/query?originLat=LAT&originLong=LNG&destinationLat=LAT&destinationLong=LNG&hour=HOUR&weekday=WEEKDAY` to generate predictions.
