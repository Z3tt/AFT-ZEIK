#!/usr/bin/env python

# This little helper is used to create jobs for running netlogo models on a torque cluster.
# Change the Configuration variables, save the file and run "python create-job.py"


# ------------------------------------------------------
# Configuration ----------------------------------------
# ------------------------------------------------------

# Name of the job (used as Torque job name and in other places)
name = 'Test1'

# Torque Jobfile variables: ----------------------------

# number of processor cores to use:
cores = 4

# number of cluster nodes (servers) to use:
nodes = 1

# directory to operate on:
directory = 'netlogo-5.0.5'

# netlogo config: -------------------------------------

# java heap size (just the number of megabyte, as integer)
heap = 1024

# model name:
name = 'Test1'

# model 