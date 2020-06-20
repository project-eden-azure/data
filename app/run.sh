sudo service nginx stop
sudo fuser -k 3033/tcp
sudo uwsgi project.ini
sudo service nginx start
tail -f project.log