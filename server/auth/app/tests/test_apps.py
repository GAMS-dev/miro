import os
import shutil
import pytest
import requests
import psycopg2
from fastapi.testclient import TestClient

from ..main import app

client = TestClient(app)

VALID_AUTH_TUPLE = (os.environ["ENGINE_USER"], os.environ["ENGINE_PASSWORD"])
SPECS_FILE_PATH = "/home/miro/admin/data/specs.yaml"


def get_db_cursor():
    conn = psycopg2.connect(dbname=os.environ['GMS_MIRO_DATABASE'], user=os.environ['GMS_MIRO_DATABASE_USER'], host=os.environ['GMS_MIRO_DATABASE_HOST'],
                            password=os.environ['GMS_MIRO_DATABASE_PWD'], port=5432)
    return conn, conn.cursor()


def get_scen_metadata(app_id: str):
    conn, cur = get_db_cursor()
    cur.execute("SELECT schema_name FROM information_schema.schemata")
    db_schemas = [schema[0] for schema in cur.fetchall()]
    schema_name = "M_" + app_id.upper()
    if schema_name not in db_schemas:
        cur.close()
        conn.close()
        return []
    try:
        cur.execute(f'SELECT * FROM "{schema_name}"."_sys_metadata_"')
        scen_data = cur.fetchall()
    except psycopg2.errors.UndefinedTable:
        scen_data = []
    cur.close()
    conn.close()
    return scen_data


@pytest.fixture()
def cleanup():
    requests.post(f"{os.environ['ENGINE_URL']}/namespaces/{os.environ['ENGINE_NS']}/user-groups?label=mygroup",
                  auth=VALID_AUTH_TUPLE)
    os.remove(SPECS_FILE_PATH)
    with open(SPECS_FILE_PATH, "w") as f:
        f.write("specs:\n- accessGroups:\n  - admins\n  containerEnv:\n    MIRO_API_VERSION: '1'\n  containerVolumes:\n  - :/home/miro/admin/models\n  - :/home/miro/admin/data\n  description: MIRO Server Admin Panel\n  displayName: Admin\n  id: admin\n  logoURL: ~\n")
    yield
    conn, cur = get_db_cursor()
    for app_id in ["transport", "transport_test"]:
        cur.execute(f"DROP SCHEMA IF EXISTS M_{app_id.upper()} CASCADE")
        requests.delete(f"{os.environ['ENGINE_URL']}/namespaces/{os.environ['ENGINE_NS']}/models/{app_id}",
                        auth=VALID_AUTH_TUPLE)
        try:
            shutil.rmtree(f'/home/miro/admin/models/{app_id}')
        except FileNotFoundError:
            pass
        try:
            shutil.rmtree(f'/home/miro/admin/data/data_{app_id}')
        except FileNotFoundError:
            pass
    cur.close()
    conn.close()
    requests.delete(f"{os.environ['ENGINE_URL']}/namespaces/{os.environ['ENGINE_NS']}/user-groups/?label=mygroup",
                    auth=VALID_AUTH_TUPLE)


class TestApps:
    def test_post_app(self, cleanup):
        response = client.get("/api/apps/",
                              auth=VALID_AUTH_TUPLE)
        assert response.json() == []
        validMiroAppPath = "tests/data/transport.miroapp"
        response = client.post("/api/apps/",
                               files={'app_data': open(
                                   validMiroAppPath, 'rb')},
                               data={
                                   'overwrite_data': True
                               })
        print(response.json())
        assert response.status_code == 401
        response = client.post("/api/apps/",
                               files={"app_data": open(
                                   validMiroAppPath, "rb")},
                               data={
                                   "overwrite_data": True
                               },
                               headers={"Authorization": "Bearer imnotvalid"})
        print(response.json())
        assert response.status_code == 401

        # not valid zip file
        response = client.post("/api/apps/",
                               files={"app_data": open(
                                   "tests/data/bad.miroapp", "rb")},
                               data={
                                   "overwrite_data": True
                               },
                               auth=VALID_AUTH_TUPLE)
        print(response.json())
        assert response.status_code == 400
        assert "Not a valid MIRO app file" in response.json()['detail']

        # not deployed for multi user
        response = client.post("/api/apps/",
                               files={"app_data": open(
                                   "tests/data/bad2.miroapp", "rb")},
                               data={
                                   "overwrite_data": True
                               },
                               auth=VALID_AUTH_TUPLE)
        print(response.json())
        assert response.status_code == 400
        assert "not deployed for multi-user environment" in response.json()[
            'detail']

        response = client.post("/api/apps/",
                               files={"app_data": open(
                                   validMiroAppPath, "rb")},
                               data={
                                   "overwrite_data": True
                               },
                               auth=VALID_AUTH_TUPLE)
        print(response.json())
        assert response.status_code == 201
        scen_data = get_scen_metadata("transport")
        assert len(scen_data) == 1
        creation_time = scen_data[0][3]
        response = client.delete("/api/apps/transport", auth=VALID_AUTH_TUPLE)
        print(response.json())
        assert response.status_code == 200
        response = client.post("/api/apps/",
                               files={"app_data": open(
                                   validMiroAppPath, "rb")},
                               data={
                                   "overwrite_data": True
                               },
                               auth=VALID_AUTH_TUPLE)
        print(response.json())
        assert response.status_code == 201
        scen_data = get_scen_metadata("transport")
        creation_time_new = scen_data[0][3]
        assert creation_time_new > creation_time
        creation_time = creation_time_new

        response = client.delete("/api/apps/transport", auth=VALID_AUTH_TUPLE)
        print(response.json())
        assert response.status_code == 200
        response = client.post("/api/apps/",
                               files={"app_data": open(
                                   validMiroAppPath, "rb")},
                               data={
                                   "overwrite_data": False
                               },
                               auth=VALID_AUTH_TUPLE)
        print(response.json())
        assert response.status_code == 201
        scen_data = get_scen_metadata("transport")
        creation_time_new = scen_data[0][3]
        assert creation_time_new == creation_time

        response = client.get("/api/apps/",
                              auth=VALID_AUTH_TUPLE)
        assert response.json() == [
            {'id': 'transport', 'displayName': 'transport', 'description': '', 'accessGroups': []}]
        response = client.post("/api/apps/",
                               files={"app_data": open(
                                   validMiroAppPath, "rb")},
                               data={
                                   "app_id": "transport_test",
                                   "display_name": "My custom transport",
                                   "description": "This is my custom transport app",
                                   "access_groups": ["mygroup"],
                                   "overwrite_data": True
                               },
                               auth=VALID_AUTH_TUPLE)
        print(response.json())
        assert response.status_code == 201
        response = client.get("/api/apps/",
                              auth=VALID_AUTH_TUPLE)
        assert response.json() == [
            {'id': 'transport', 'displayName': 'transport',
                'description': '', 'accessGroups': []},
            {'id': 'transport_test', 'displayName': 'My custom transport', 'description': 'This is my custom transport app', 'accessGroups': ["MYGROUP"]}]
        assert len(get_scen_metadata("transport_test")) == 1
        # migrating database currently not possible
        response = client.post("/api/apps/",
                               files={"app_data": open(
                                   "tests/data/pickstock.miroapp", "rb")},
                               data={
                                   "app_id": "transport_test",
                                   "display_name": "bla",
                                   "description": "test",
                                   "access_groups": [],
                                   "overwrite_data": True
                               },
                               auth=VALID_AUTH_TUPLE)
        print(response.json())
        assert response.status_code == 409
        response = client.get("/api/apps/",
                              auth=VALID_AUTH_TUPLE)
        assert response.json() == [
            {'id': 'transport', 'displayName': 'transport',
                'description': '', 'accessGroups': []},
            {'id': 'transport_test', 'displayName': 'My custom transport', 'description': 'This is my custom transport app', 'accessGroups': ["MYGROUP"]}]
        assert len(get_scen_metadata("transport_test")) == 1

    def test_delete_app(self, cleanup):
        response = client.get("/api/apps/",
                              auth=VALID_AUTH_TUPLE)
        assert response.json() == []
        response = client.post("/api/apps/",
                               files={"app_data": open(
                                   "tests/data/transport.miroapp", "rb")},
                               data={
                                   "overwrite_data": True
                               },
                               auth=VALID_AUTH_TUPLE)
        print(response.json())
        assert response.status_code == 201
        assert len(get_scen_metadata("transport")) == 1
        response = client.get("/api/apps/",
                              auth=VALID_AUTH_TUPLE)
        assert response.json() == [
            {'id': 'transport', 'displayName': 'transport', 'description': '', 'accessGroups': []}]
        response = client.delete("/api/apps/transport", auth=VALID_AUTH_TUPLE)
        print(response.json())
        assert response.status_code == 200
        response = client.get("/api/apps/",
                              auth=VALID_AUTH_TUPLE)
        assert response.json() == []
        assert len(get_scen_metadata("transport")) == 1
        response = client.post("/api/apps/",
                               files={"app_data": open(
                                   "tests/data/transport.miroapp", "rb")},
                               data={
                                   "overwrite_data": True
                               },
                               auth=VALID_AUTH_TUPLE)
        print(response.json())
        assert response.status_code == 201
        response = client.delete(
            "/api/apps/transport", params={"delete_data": True}, auth=VALID_AUTH_TUPLE)
        print(response.json())
        assert response.status_code == 200
        assert len(get_scen_metadata("transport")) == 0

    def test_update_app(self, cleanup):
        response = client.post("/api/apps/",
                               files={"app_data": open(
                                   "tests/data/transport.miroapp", "rb")},
                               data={
                                   "overwrite_data": True
                               },
                               auth=VALID_AUTH_TUPLE)
        print(response.json())
        assert response.status_code == 201
        response = client.get("/api/apps/",
                              auth=VALID_AUTH_TUPLE)
        assert response.json() == [
            {'id': 'transport', 'displayName': 'transport', 'description': '', 'accessGroups': []}]
        scen_data = get_scen_metadata("transport")
        assert len(scen_data) == 1
        creation_time = scen_data[0][3]
        response = client.put("/api/apps/transport",
                              files={"app_data": open(
                                  "tests/data/transport.miroapp", "rb")},
                              data={
                                  "display_name": "My custom transport",
                                  "description": "This is my custom transport app",
                                  "access_groups": ["mygroup", "admins"],
                                  "overwrite_data": False
                              },
                              auth=VALID_AUTH_TUPLE)
        print(response.json())
        assert response.status_code == 400
        assert "admins" in response.json()["detail"]

        response = client.put("/api/apps/transport",
                              files={"app_data": open(
                                  "tests/data/transport.miroapp", "rb")},
                              data={
                                  "display_name": "My custom transport",
                                  "description": "This is my custom transport app",
                                  "access_groups": ["mygroup"],
                                  "overwrite_data": False
                              },
                              auth=VALID_AUTH_TUPLE)
        print(response.json())
        assert response.status_code == 200

        response = client.get("/api/apps/",
                              auth=VALID_AUTH_TUPLE)
        assert response.json() == [
            {'id': 'transport', 'displayName': 'My custom transport', 'description': 'This is my custom transport app', 'accessGroups': ["MYGROUP"]}]
        scen_data = get_scen_metadata("transport")
        assert len(scen_data) == 1
        new_creation_time = scen_data[0][3]
        assert new_creation_time == creation_time
        creation_time = new_creation_time

        response = client.put("/api/apps/transport",
                              files={"app_data": open(
                                  "tests/data/transport.miroapp", "rb")},
                              data={
                                  "display_name": "Test123",
                                  "description": "This is my custom transport app",
                                  "overwrite_data": True
                              },
                              auth=VALID_AUTH_TUPLE)
        print(response.json())
        assert response.status_code == 200

        response = client.get("/api/apps/",
                              auth=VALID_AUTH_TUPLE)
        assert response.json() == [
            {'id': 'transport', 'displayName': 'Test123', 'description': 'This is my custom transport app', 'accessGroups': []}]

        scen_data = get_scen_metadata("transport")
        assert len(scen_data) == 1
        new_creation_time = scen_data[0][3]
        assert new_creation_time > creation_time
