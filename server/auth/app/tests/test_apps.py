import os
import shutil
import pytest
import requests
from fastapi.testclient import TestClient

from ..main import app
from .util import (
    settings,
    get_db_cursor,
    get_db_prefix,
    get_scen_metadata,
    reset_app_config_file,
)
from ..utils.app_utils import get_apps_raw

client = TestClient(app)


@pytest.fixture()
def cleanup():
    requests.post(
        f"{settings['ENGINE_URL']}/namespaces/{settings['ENGINE_NS']}/user-groups?label=mygroup",
        auth=settings["VALID_AUTH_TUPLE"],
    )
    os.remove(settings["SPECS_FILE_PATH"])
    reset_app_config_file()
    yield
    conn, cur = get_db_cursor()
    for app_id in ["transport", "transport_test"]:
        cur.execute(
            f'DROP SCHEMA IF EXISTS "{get_db_prefix(cur)}{app_id.upper()}" CASCADE'
        )
        requests.delete(
            f"{settings['ENGINE_URL']}/namespaces/{settings['ENGINE_NS']}/models/{app_id}",
            auth=settings["VALID_AUTH_TUPLE"],
        )
        if "KUBERNETES_SERVICE_HOST" in os.environ:
            try:
                shutil.rmtree(f"/home/miro/admin/mnt/data/{app_id}")
            except FileNotFoundError:
                pass
        else:
            try:
                shutil.rmtree(f"/home/miro/admin/models/{app_id}")
            except FileNotFoundError:
                pass
            try:
                shutil.rmtree(f"/home/miro/admin/data/data_{app_id}")
            except FileNotFoundError:
                pass
    conn.commit()
    cur.close()
    conn.close()
    reset_app_config_file()
    requests.delete(
        f"{settings['ENGINE_URL']}/namespaces/{settings['ENGINE_NS']}/user-groups/?label=mygroup",
        auth=settings["VALID_AUTH_TUPLE"],
    )


class TestApps:
    def test_get_apps_raw(self, cleanup):
        with open(settings["SPECS_FILE_PATH"], "w") as f:
            f.write(
                "specs:\n- accessGroups:\n  - admins\n  containerEnv:\n    MIRO_API_VERSION: '1'\n  containerVolumes:\n  - :/home/miro/admin/models\n  - :/home/miro/admin/data\n  description: MIRO Server Admin Panel\n  displayName: Admin\n  id: admin\n  logoURL: ~\n- id: transport\n  displayName: transport\n  description: ''\n  logoURL: default_logo.png\n  containerVolumes:\n  - /transport:/home/miro/app/model/transport:ro\n  - /data_transport:/home/miro/app/data\n  containerEnv:\n    MIRO_MODEL_PATH: /home/miro/app/model/transport/transport.gms\n    MIRO_DATA_DIR: /home/miro/app/data\n  accessGroups:\n  - group 1\n  - group 2\n- id: pickstock\n  displayName: pickstock\n  description: ''\n  logoURL: default_logo.png\n  containerVolumes:\n  - /pickstock:/home/miro/app/model/pickstock:ro\n  - /data_transport:/home/miro/app/data\n  containerEnv:\n    MIRO_MODEL_PATH: /home/miro/app/model/pickstock/pickstock.gms\n    MIRO_DATA_DIR: /home/miro/app/data\n  accessGroups:\n  - group 1\n- id: sudoku\n  displayName: sudoku\n  description: ''\n  logoURL: default_logo.png\n  containerVolumes:\n  - /sudoku:/home/miro/app/model/sudoku:ro\n  - /data_transport:/home/miro/app/data\n  containerEnv:\n    MIRO_MODEL_PATH: /home/miro/app/model/sudoku/sudoku.gms\n    MIRO_DATA_DIR: /home/miro/app/data\n  accessGroups:\n  - group 2\n- id: tsp\n  displayName: tsp\n  description: ''\n  logoURL: default_logo.png\n  containerVolumes:\n  - /tsp:/home/miro/app/model/tsp:ro\n  - /data_transport:/home/miro/app/data\n  containerEnv:\n    MIRO_MODEL_PATH: /home/miro/app/model/tsp/tsp.gms\n    MIRO_DATA_DIR: /home/miro/app/data\n"
            )
        assert [app["id"] for app in get_apps_raw(["group 1", "GROUP 2"])] == [
            "transport",
            "pickstock",
            "sudoku",
            "tsp",
        ]
        assert [app["id"] for app in get_apps_raw(["group 1"])] == [
            "transport",
            "pickstock",
            "tsp",
        ]
        assert [app["id"] for app in get_apps_raw(["group 2"])] == [
            "transport",
            "sudoku",
            "tsp",
        ]
        assert [app["id"] for app in get_apps_raw(["users"])] == ["tsp"]
        assert [app["id"] for app in get_apps_raw([])] == ["tsp"]

    def test_post_app(self, cleanup):
        response = client.get("/api/apps/", auth=settings["VALID_AUTH_TUPLE"])
        assert response.json() == []
        assert response.headers["x-total"] == "0"
        assert response.headers["x-total-pages"] == "0"
        assert response.headers["x-next-page"] == ""
        assert response.headers["x-prev-page"] == ""
        validMiroAppPath = "tests/data/transport.miroapp"
        response = client.post(
            "/api/apps/",
            files={"app_data": open(validMiroAppPath, "rb")},
            data={"overwrite_data": True},
        )
        print(response.json())
        assert response.status_code == 401
        response = client.post(
            "/api/apps/",
            files={"app_data": open(validMiroAppPath, "rb")},
            data={"overwrite_data": True},
            headers={"Authorization": "Bearer imnotvalid"},
        )
        print(response.json())
        assert response.status_code == 401

        # not valid zip file
        response = client.post(
            "/api/apps/",
            files={"app_data": open("tests/data/bad.miroapp", "rb")},
            data={"overwrite_data": True},
            auth=settings["VALID_AUTH_TUPLE"],
        )
        print(response.json())
        assert response.status_code == 400
        assert "Not a valid MIRO app file" in response.json()["detail"]

        # not deployed for multi user
        response = client.post(
            "/api/apps/",
            files={"app_data": open("tests/data/bad2.miroapp", "rb")},
            data={"overwrite_data": True},
            auth=settings["VALID_AUTH_TUPLE"],
        )
        print(response.json())
        assert response.status_code == 400
        assert "not deployed for multi-user environment" in response.json()["detail"]

        # not signed
        if "KUBERNETES_SERVICE_HOST" in os.environ:
            response = client.post(
                "/api/apps/",
                files={"app_data": open("tests/data/bad3.miroapp", "rb")},
                data={"overwrite_data": True},
                auth=settings["VALID_AUTH_TUPLE"],
            )
            print(response.json())
            assert response.status_code == 400
            assert "App is not signed" in response.json()["detail"]

        response = client.post(
            "/api/apps/",
            files={"app_data": open(validMiroAppPath, "rb")},
            data={"overwrite_data": True},
            auth=settings["VALID_AUTH_TUPLE"],
        )
        print(response.json())
        assert response.status_code == 201
        scen_data = get_scen_metadata("transport")
        assert len(scen_data) == 1
        creation_time = scen_data[0][3]
        response = client.delete(
            "/api/apps/transport", auth=settings["VALID_AUTH_TUPLE"]
        )
        print(response.json())
        assert response.status_code == 200
        response = client.post(
            "/api/apps/",
            files={"app_data": open(validMiroAppPath, "rb")},
            data={"overwrite_data": True},
            auth=settings["VALID_AUTH_TUPLE"],
        )
        print(response.json())
        assert response.status_code == 201
        scen_data = get_scen_metadata("transport")
        creation_time_new = scen_data[0][3]
        assert creation_time_new > creation_time
        creation_time = creation_time_new

        response = client.delete(
            "/api/apps/transport", auth=settings["VALID_AUTH_TUPLE"]
        )
        print(response.json())
        assert response.status_code == 200
        response = client.post(
            "/api/apps/",
            files={"app_data": open(validMiroAppPath, "rb")},
            data={"overwrite_data": False},
            auth=settings["VALID_AUTH_TUPLE"],
        )
        print(response.json())
        assert response.status_code == 201
        scen_data = get_scen_metadata("transport")
        creation_time_new = scen_data[0][3]
        assert creation_time_new == creation_time

        response = client.get("/api/apps/", auth=settings["VALID_AUTH_TUPLE"])
        assert response.json() == [
            {
                "id": "transport",
                "display_name": "Transport test app",
                "description": "Transport app for UI tests",
                "access_groups": [],
                "environment": {},
                "version": "1.0.0",
                "authors": ["GAMS Development Corp."],
            }
        ]
        # test some bad environment configurations
        response = client.post(
            "/api/apps/",
            files={"app_data": open(validMiroAppPath, "rb")},
            data={
                "app_id": "transport_test",
                "display_name": "My custom transport",
                "description": "This is my custom transport app",
                "environment": '{"ENV_NAMe1":{"description":"bla","value":"test123"},"ENV_NAME2":{"value":"bumbum"}}',
                "access_groups": ["mygroup"],
                "overwrite_data": True,
            },
            auth=settings["VALID_AUTH_TUPLE"],
        )
        print(response.json())
        assert response.status_code == 422
        assert "ENV_NAMe1" == response.json()["detail"][0]["loc"][0]
        assert "string_pattern_mismatch" == response.json()["detail"][0]["type"]
        response = client.post(
            "/api/apps/",
            files={"app_data": open(validMiroAppPath, "rb")},
            data={
                "app_id": "transport_test",
                "display_name": "My custom transport",
                "description": "This is my custom transport app",
                "environment": '{"ENV_NAME1":{"description":"bla","value":"test123"},"ENV_NAME2":{}}',
                "access_groups": ["mygroup"],
                "overwrite_data": True,
            },
            auth=settings["VALID_AUTH_TUPLE"],
        )
        print(response.json())
        assert response.status_code == 422
        assert "ENV_NAME2" == response.json()["detail"][0]["loc"][0]
        assert "value" == response.json()["detail"][0]["loc"][1]
        assert "missing" == response.json()["detail"][0]["type"]

        response = client.post(
            "/api/apps/",
            files={"app_data": open(validMiroAppPath, "rb")},
            data={
                "app_id": "transport_test",
                "display_name": "My custom transport",
                "description": "This is my custom transport app",
                "environment": '{"ENV_NAME1":{"description":"bla","value":"test123"},"ENV_NAME2":{"value":"bumbum"}}',
                "access_groups": ["mygroup"],
                "overwrite_data": True,
            },
            auth=settings["VALID_AUTH_TUPLE"],
        )
        print(response.json())
        assert response.status_code == 201
        response = client.get("/api/apps/", auth=settings["VALID_AUTH_TUPLE"])
        assert response.json() == [
            {
                "id": "transport",
                "display_name": "Transport test app",
                "description": "Transport app for UI tests",
                "access_groups": [],
                "environment": {},
                "version": "1.0.0",
                "authors": ["GAMS Development Corp."],
            },
            {
                "id": "transport_test",
                "display_name": "My custom transport",
                "description": "This is my custom transport app",
                "access_groups": ["mygroup"],
                "environment": {
                    "ENV_NAME1": {"value": "test123", "description": "bla"},
                    "ENV_NAME2": {"value": "bumbum", "description": ""},
                },
                "version": "1.0.0",
                "authors": ["GAMS Development Corp."],
            },
        ]
        assert len(get_scen_metadata("transport_test")) == 1

        # adding same app twice should cause 400
        response = client.post(
            "/api/apps/",
            files={"app_data": open("tests/data/pickstock.miroapp", "rb")},
            data={
                "app_id": "transport_test",
                "display_name": "asd",
                "description": "def",
                "access_groups": [],
                "overwrite_data": True,
            },
            auth=settings["VALID_AUTH_TUPLE"],
        )
        print(response.json())
        assert response.status_code == 400
        response = client.get("/api/apps/", auth=settings["VALID_AUTH_TUPLE"])
        assert response.json() == [
            {
                "id": "transport",
                "display_name": "Transport test app",
                "description": "Transport app for UI tests",
                "access_groups": [],
                "environment": {},
                "version": "1.0.0",
                "authors": ["GAMS Development Corp."],
            },
            {
                "id": "transport_test",
                "display_name": "My custom transport",
                "description": "This is my custom transport app",
                "access_groups": ["mygroup"],
                "environment": {
                    "ENV_NAME1": {"value": "test123", "description": "bla"},
                    "ENV_NAME2": {"value": "bumbum", "description": ""},
                },
                "version": "1.0.0",
                "authors": ["GAMS Development Corp."],
            },
        ]
        assert len(get_scen_metadata("transport_test")) == 1
        assert response.headers["x-total"] == "2"
        assert response.headers["x-per-page"] == "20"
        assert response.headers["x-total-pages"] == "1"
        response = client.get(
            "/api/apps/?page=2&per_page=1", auth=settings["VALID_AUTH_TUPLE"]
        )
        assert response.json() == [
            {
                "id": "transport_test",
                "display_name": "My custom transport",
                "description": "This is my custom transport app",
                "access_groups": ["mygroup"],
                "environment": {
                    "ENV_NAME1": {"value": "test123", "description": "bla"},
                    "ENV_NAME2": {"value": "bumbum", "description": ""},
                },
                "version": "1.0.0",
                "authors": ["GAMS Development Corp."],
            },
        ]
        assert response.headers["x-total"] == "2"
        assert 'rel="prev"' in response.headers["link"]
        assert 'rel="first"' in response.headers["link"]
        assert 'rel="last"' in response.headers["link"]
        assert 'rel="next"' not in response.headers["link"]
        assert "/api/apps/?page=1&per_page=1" in response.headers["link"]

        response = client.delete(
            "/api/apps/transport_test", auth=settings["VALID_AUTH_TUPLE"]
        )
        print(response.json())
        assert response.status_code == 200
        # migrating database currently not possible
        response = client.post(
            "/api/apps/",
            files={"app_data": open("tests/data/pickstock.miroapp", "rb")},
            data={
                "app_id": "transport_test",
                "display_name": "bla",
                "description": "test",
                "access_groups": [],
                "overwrite_data": True,
            },
            auth=settings["VALID_AUTH_TUPLE"],
        )
        print(response.json())
        assert response.status_code == 409
        response = client.get("/api/apps/", auth=settings["VALID_AUTH_TUPLE"])
        assert response.json() == [
            {
                "id": "transport",
                "display_name": "Transport test app",
                "description": "Transport app for UI tests",
                "access_groups": [],
                "environment": {},
                "version": "1.0.0",
                "authors": ["GAMS Development Corp."],
            }
        ]
        assert len(get_scen_metadata("transport")) == 1

    def test_delete_app(self, cleanup):
        response = client.get("/api/apps/", auth=settings["VALID_AUTH_TUPLE"])
        assert response.json() == []
        response = client.post(
            "/api/apps/",
            files={"app_data": open("tests/data/transport.miroapp", "rb")},
            data={"overwrite_data": True},
            auth=settings["VALID_AUTH_TUPLE"],
        )
        print(response.json())
        assert response.status_code == 201
        assert len(get_scen_metadata("transport")) == 1
        response = client.get("/api/apps/", auth=settings["VALID_AUTH_TUPLE"])
        assert response.json() == [
            {
                "id": "transport",
                "display_name": "Transport test app",
                "description": "Transport app for UI tests",
                "access_groups": [],
                "environment": {},
                "version": "1.0.0",
                "authors": ["GAMS Development Corp."],
            }
        ]
        response = client.delete(
            "/api/apps/transport", auth=settings["VALID_AUTH_TUPLE"]
        )
        print(response.json())
        assert response.status_code == 200
        response = client.get("/api/apps/", auth=settings["VALID_AUTH_TUPLE"])
        assert response.json() == []
        assert len(get_scen_metadata("transport")) == 1
        response = client.post(
            "/api/apps/",
            files={"app_data": open("tests/data/transport.miroapp", "rb")},
            data={"overwrite_data": True},
            auth=settings["VALID_AUTH_TUPLE"],
        )
        print(response.json())
        assert response.status_code == 201
        response = client.delete(
            "/api/apps/transport",
            params={"delete_data": True},
            auth=settings["VALID_AUTH_TUPLE"],
        )
        print(response.json())
        assert response.status_code == 200
        assert len(get_scen_metadata("transport")) == 0

    def test_update_app(self, cleanup):
        response = client.post(
            "/api/apps/",
            files={"app_data": open("tests/data/transport.miroapp", "rb")},
            data={"overwrite_data": True},
            auth=settings["VALID_AUTH_TUPLE"],
        )
        print(response.json())
        assert response.status_code == 201
        response = client.get("/api/apps/", auth=settings["VALID_AUTH_TUPLE"])
        assert response.json() == [
            {
                "id": "transport",
                "display_name": "Transport test app",
                "description": "Transport app for UI tests",
                "access_groups": [],
                "environment": {},
                "version": "1.0.0",
                "authors": ["GAMS Development Corp."],
            }
        ]
        scen_data = get_scen_metadata("transport")
        assert len(scen_data) == 1
        creation_time = scen_data[0][3]
        response = client.put(
            "/api/apps/transport",
            files={"app_data": open("tests/data/transport.miroapp", "rb")},
            data={
                "display_name": "My custom transport",
                "description": "This is my custom transport app",
                "access_groups": ["mygroup", "admins"],
                "overwrite_data": False,
            },
            auth=settings["VALID_AUTH_TUPLE"],
        )
        print(response.json())
        assert response.status_code == 400
        assert "admins" in response.json()["detail"]

        response = client.put(
            "/api/apps/transport",
            files={"app_data": open("tests/data/transport.miroapp", "rb")},
            data={
                "display_name": "My custom transport",
                "description": "This is my custom transport app",
                "access_groups": ["mygroup"],
                "overwrite_data": False,
            },
            auth=settings["VALID_AUTH_TUPLE"],
        )
        print(response.json())
        assert response.status_code == 200

        response = client.get("/api/apps/", auth=settings["VALID_AUTH_TUPLE"])
        assert response.json() == [
            {
                "id": "transport",
                "display_name": "My custom transport",
                "description": "This is my custom transport app",
                "access_groups": ["mygroup"],
                "environment": {},
                "version": "1.0.0",
                "authors": ["GAMS Development Corp."],
            }
        ]
        scen_data = get_scen_metadata("transport")
        assert len(scen_data) == 1
        new_creation_time = scen_data[0][3]
        assert new_creation_time == creation_time
        creation_time = new_creation_time

        response = client.put(
            "/api/apps/transport",
            files={"app_data": open("tests/data/transport.miroapp", "rb")},
            data={
                "display_name": "Test123",
                "description": "This is my custom transport app",
                "environment": '{"LALA": {"description": "huhu", "value": "haihai"}}',
                "overwrite_data": True,
            },
            auth=settings["VALID_AUTH_TUPLE"],
        )
        print(response.json())
        assert response.status_code == 200

        response = client.get("/api/apps/", auth=settings["VALID_AUTH_TUPLE"])
        assert response.json() == [
            {
                "id": "transport",
                "display_name": "Test123",
                "description": "This is my custom transport app",
                "access_groups": [],
                "environment": {"LALA": {"description": "huhu", "value": "haihai"}},
                "version": "1.0.0",
                "authors": ["GAMS Development Corp."],
            }
        ]

        scen_data = get_scen_metadata("transport")
        assert len(scen_data) == 1
        new_creation_time = scen_data[0][3]
        assert new_creation_time > creation_time

        response = client.put(
            "/api/apps/transport",
            files={"app_data": open("tests/data/pickstock.miroapp", "rb")},
            data={"display_name": "asd", "description": "def", "overwrite_data": True},
            auth=settings["VALID_AUTH_TUPLE"],
        )
        print(response.json())
        assert response.status_code == 409

        response = client.get("/api/apps/", auth=settings["VALID_AUTH_TUPLE"])
        assert response.json() == [
            {
                "id": "transport",
                "display_name": "Test123",
                "description": "This is my custom transport app",
                "access_groups": [],
                "environment": {"LALA": {"value": "haihai", "description": "huhu"}},
                "version": "1.0.0",
                "authors": ["GAMS Development Corp."],
            }
        ]
