# syntax=docker/dockerfile:1

FROM python:3.8-slim-buster
WORKDIR /app
COPY requirement.txt requirement.txt 
RUN pip3 install -r requirement.txt
COPY . . 
CMD ["python3", "sample.py"]
