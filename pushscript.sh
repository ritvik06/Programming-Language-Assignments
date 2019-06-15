#!/bin/sh
git pull origin master
git add .
read -p "Enter your commit message: " message
git config user.name 'ritvik06'
git commit -m message
git push origin master
