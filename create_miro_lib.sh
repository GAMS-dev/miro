#!/bin/sh

cd model
zip -r transport.conf.unzip conf_transport
zip miroDemoApps.zip pickstock/pickstock.gms pickstock/dowjones2016.csv pickstock/conf/pickstock.json pickstock/static/pickstock.png kport/kport.gms kport/conf/kport.json kport/static/kport.jpg transport/transport.gms transport/conf/transport.json transport/custom_renderer/* transport_live/transport_live.gms transport_live/conf/transport_live.json transport_live/custom_renderer/*
mv miroDemoApps.zip ../doc
cd ..