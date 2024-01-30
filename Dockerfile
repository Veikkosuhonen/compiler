FROM ubuntu:latest

WORKDIR /opt/app-root/src

RUN apt-get update
RUN apt-get install -y binutils
RUN apt-get install build-essential -y

COPY . .
