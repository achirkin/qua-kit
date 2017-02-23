#!/bin/bash

psql -f sirenDBcreate.sql
psql -d sirendb -f sirenDBconfigure.sql
