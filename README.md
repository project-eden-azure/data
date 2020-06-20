Datasets and modelling-related work.

### Predicting Grab trip ETAs

This is code related to Project Eden's entry into the Azure/Grab hackathon.

We provide a REST endpoint to obtain predictions for the ETA in seconds.

**Predicting a trip** 

![](https://i.imgur.com/mHZhEPy.png)

**Result**

![](https://i.imgur.com/PZf6MPz.png)

Result is returned in JSON.

Access the endpoint at: [http://52.189.235.58/query?originLat=1.3644&originLong=103.9915&destinationLat=1.2966&destinationLong=103.7764&hour=1&weekday=1](http://52.189.235.58/query?originLat=1.3644&originLong=103.9915&destinationLat=1.2966&destinationLong=103.7764&hour=1&weekday=1)

### Installation and deployment

#### Install Python

View instructions on how to install Python for your operating system here: https://www.python.org/downloads/.

#### Install pip

Ensure you can run pip from the command-line.

View instructions on how to install pip here: https://packaging.python.org/tutorials/installing-packages/

For Ubuntu, python-pip is in the universe repository.

```
sudo apt-get install software-properties-common
sudo apt-add-repository universe
sudo apt-get update
sudo apt-get install python3-pip
```

#### Install Flask

View instructions on how to install Flask here: http://flask.pocoo.org/docs/0.12/installation/

####  Install R

Select a mirror to install R here: https://cran.r-project.org/mirrors.html

For Ubuntu, you can try these commands.

To remove R:

```
sudo apt-get remove r-base-core
```

To install R:

```
sudo apt-get install libxml2-dev
sudo echo "deb http://cran.rstudio.com/bin/linux/ubuntu xenial/" | sudo tee -a /etc/apt/sources.list
gpg --keyserver keyserver.ubuntu.com --recv-key E298A3A825C0D65DFD57CBB651716619E084DAB9
gpg -a --export E084DAB9 | sudo apt-key add -
sudo apt-get update
sudo apt-get install r-base-core
sudo apt-get -y build-dep libcurl4-gnutls-dev
sudo apt-get -y install libcurl4-gnutls-dev
```

#### Install rpy2, NGINX, uWSGI, Flask

Run `pip3 install rpy2` to install rpy2. .

See also: http://rpy.sourceforge.net/rpy2/doc-dev/html/overview.html

Ubuntu users may also wish to try `sudo apt install python3-rpy2`

To install NGINX/uWSGI (for load balancing and serving):

```
pip3 install uwsgi flask
sudo apt-get install uwsgi-plugin-python
sudo apt-get install python3-dev nginx
```

#### Install R packages

Open up R and install any required packages that you are missing from:

```
library( data.table )
library( caret )
library( mgcv )
library( pls )
library( ranger )
library( randomGLM )
library( xgboost )
library( brnn )
library( mgcv )
```

You can do this with `install.packages('package')`.

You must install xgboost version 1.0.0.2 only. You can do this with `remotes::install_version("xgboost", "1.0.0.2", quiet = FALSE)`.

#### Set up NGINX

Edit your /etc/nginx/nginx.conf file. Inside the http {} block, add these lines.
```
server {
                listen 80;
                location / {
                        include uwsgi_params;
                        uwsgi_pass 127.0.0.1:3033;
                        proxy_read_timeout 3600;
                        proxy_http_version 1.1;
                        proxy_set_header Connection "";
                }
}
```

You also need to remove `/etc/nginx/sites-enabled/default`.

#### Run

After cloning the contents of the /app folder and uploading the models into the /app/model directory, you can run the application.

```
chmod +x run.sh
./run.sh
```

Make sure your outgoing ports are open:

```
sudo iptables -I OUTPUT -o eth0 -d 0.0.0.0/0 -j ACCEPT
sudo iptables -I INPUT -i eth0 -m state --state ESTABLISHED,RELATED -j ACCEPT
```

Make sure port 80 is accepting incoming connections as well.