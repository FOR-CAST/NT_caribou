#!/bin/bash

## wbi_data_orig
rsync -avzP /mnt/projects/WBI/NT_caribou/outputs/*.tar.gz /mnt/wbi_data_orig/NT_caribou/outputs/
rsync -avzP /mnt/projects/WBI/NT_caribou/outputs/NT1/posthoc /mnt/wbi_data_orig/NT_caribou/outputs/

## narval
pushd /mnt/projects/WBI/NT_caribou/outputs/NT1
tar -vc --use-compress-program="pigz -p 16" -f posthoc.tar.gz posthoc
popd

rsync -avzP /mnt/projects/WBI/NT_caribou/outputs/*.tar.gz narval:/home/achubaty/projects/rrg-stevec/achubaty/NT_caribou/outputs/
rsync -avzP /mnt/projects/WBI/NT_caribou/outputs/NT1/posthoc.tar.gz narval:/home/achubaty/projects/rrg-stevec/achubaty/NT_caribou/outputs/
