#!/bin/sh

cd `dirname $0`

echo "Copying frontend"

rsync -avz                                              \
      --exclude=static/data                             \
      frontend-prototype/dist/                          \
      rodney.id.au:/srv/www/lorrimar.id.au/tour/        \

echo "Copying plugin lib"

rsync -avz frontend-prototype/plugin/ rodney.id.au:/srv/www/lorrimar.id.au/tour/

echo "Copying data files"

rsync -avz --delete --include '*.json' --include '*/' --exclude='*' _build/ rodney.id.au:/srv/www/lorrimar.id.au/tour/static/data/
