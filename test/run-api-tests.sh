#!/bin/sh

cd server

[ ! -d "admin/models" ] && ( mkdir admin/models || exit 1 )
chown -R 1000:1000 admin/models || exit 1
chown -R 1000:1000 release_data/data_raw || exit 1
docker run --rm --network=host -v ${PWD}/../src:/home/miro/app \
    -v ${PWD}/admin:/home/miro/admin -v ${PWD}/auth/app:/app/app \
    -v ${PWD}/release_data/data_raw:/home/miro/admin/data \
    -e ENGINE_URL=${ENGINE_URL} -e ENGINE_NS=${ENGINE_NS} \
    -e ENGINE_USER=${ENGINE_USER} -e ENGINE_PASSWORD=${ENGINE_PASSWORD} \
    -e GMS_MIRO_DATABASE_HOST=${GMS_MIRO_DATABASE_HOST} -e GMS_MIRO_DATABASE=${GMS_MIRO_DATABASE} \
    -e GMS_MIRO_DATABASE_USER=${GMS_MIRO_DATABASE_USER} -e GMS_MIRO_DATABASE_PWD=${GMS_MIRO_DATABASE_PWD} \
    miro-auth-test pytest tests/ -o cache_dir=/tmp/.pytest_cache \
        --junitxml=/home/miro/admin/data/$PYTEST_JUNIT --cov=/app --cov-report=xml:/home/miro/admin/data/$PYTEST_COV || exit 1

mv ${PWD}/release_data/data_raw/$PYTEST_JUNIT ${PWD}/release_data/data_raw/$PYTEST_COV ..
python3 miro_server.py

cd ..
