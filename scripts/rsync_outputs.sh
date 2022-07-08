#!/bin/bash
rsync -avzP /mnt/projects/WBI/NT_caribou/outputs/*.tar.gz /mnt/wbi_data_orig/NT_caribou/outputs/

rsync -avzP /mnt/projects/WBI/NT_caribou/outputs/*.tar.gz narval:/home/achubaty/projects/rrg-stevec/achubaty/NT_caribou/outputs/
