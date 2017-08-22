#!/bin/bash
for i in `ls ../data/photos/c/*.jpg`
do
echo "Processing image $i ..."
convert -thumbnail 200 $i ${i/.jpg/}.thumb.jpg
done