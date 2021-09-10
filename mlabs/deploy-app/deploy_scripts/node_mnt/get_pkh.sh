#!/usr/bin/env bash

USER=$1

cardano-cli address key-hash --payment-verification-key-file keys/$USER-payment.vkey