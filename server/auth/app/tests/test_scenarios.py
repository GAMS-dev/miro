import os
import re
import datetime
import zipfile
import io
import json
import shutil
import pytest
import requests
from fastapi.testclient import TestClient

from ..main import app
from .util import (
    delete_user,
    get_scen_metadata,
    lock_scenario,
    settings,
    get_db_cursor,
    get_db_prefix,
    register_transport,
    invite_user,
    delete_user,
    reset_app_config_file,
)

client = TestClient(app)


@pytest.fixture()
def cleanup():
    group_labels = ["mygroup", "another_group"]
    for group_label in group_labels:
        requests.post(
            f"{settings['ENGINE_URL']}/namespaces/{settings['ENGINE_NS']}/user-groups?label={group_label}",
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
    for group_label in group_labels:
        requests.delete(
            f"{settings['ENGINE_URL']}/namespaces/{settings['ENGINE_NS']}/user-groups?label={group_label}",
            auth=settings["VALID_AUTH_TUPLE"],
        )
    delete_user("mirotests_auth_1", allow_fail=True)


class TestScenarios:
    def test_post_scenario(self, cleanup):
        response = client.post(
            "/api/scenarios/idontexist/",
            files={"scenario_data": open("tests/data/transport.miroscen", "rb")},
            data={"overwrite_data": True},
        )
        print(response.json())
        assert response.status_code == 401
        response = client.post(
            "/api/scenarios/idontexist/",
            files={"scenario_data": open("tests/data/transport.miroscen", "rb")},
            data={"overwrite_data": True},
            headers={"Authorization": "Bearer imnotvalid"},
        )
        print(response.json())
        assert response.status_code == 401
        response = client.post(
            "/api/scenarios/idontexist/",
            files={"scenario_data": open("tests/data/transport.miroscen", "rb")},
            data={"overwrite_data": True},
            auth=settings["VALID_AUTH_TUPLE"],
        )
        print(response.json())
        assert response.status_code == 404

        # user should not see app if she is not member of group
        register_transport(client, ["mygroup"])
        invite_user("mirotests_auth_1", 1)

        response = client.post(
            "/api/scenarios/transport/",
            files={"scenario_data": open("tests/data/transport.miroscen", "rb")},
            data={"overwrite_data": True},
            auth=("mirotests_auth_1", "mirotests_auth_11234"),
        )
        print(response.json())
        assert response.status_code == 404

        delete_user("mirotests_auth_1")
        invite_user("mirotests_auth_1", 1, group="mygroup")
        response = client.post(
            "/api/scenarios/transport/",
            files={"scenario_data": open("tests/data/transport.miroscen", "rb")},
            data={
                "overwrite_data": True,
                "read_perm": ["mirotests_auth_1", "#invalidgroup"],
            },
            auth=("mirotests_auth_1", "mirotests_auth_11234"),
        )
        assert "#invalidgroup" in response.json()["detail"]
        assert response.status_code == 400

        response = client.post(
            "/api/scenarios/transport/",
            files={"scenario_data": open("tests/data/transport.miroscen", "rb")},
            data={"overwrite_data": True, "read_perm": ["mirotests_auth_1", "mygroup"]},
            auth=("mirotests_auth_1", "mirotests_auth_11234"),
        )
        assert "mygroup" in response.json()["detail"]
        assert response.status_code == 400

        response = client.post(
            "/api/scenarios/transport/",
            files={"scenario_data": open("tests/data/transport.miroscen", "rb")},
            data={
                "overwrite_data": True,
                "read_perm": ["mirotests_auth_1", "#mygroup"],
                "write_perm": ["mirotests_auth_1"],
            },
            auth=("mirotests_auth_1", "mirotests_auth_11234"),
        )
        print(response.json())
        assert response.status_code == 201

        scen_metadata = get_scen_metadata("transport")

        scen_found = False
        for scenario in scen_metadata:
            if scenario[1] == "mirotests_auth_1" and scenario[2] == "My test":
                assert set(scenario[5][1:-1].split(",")) == {
                    "mirotests_auth_1",
                    "#mygroup",
                }
                assert set(scenario[6][1:-1].split(",")) == {"mirotests_auth_1"}
                assert set(scenario[7][1:-1].split(",")) == {
                    "mirotests_auth_1",
                    "#mygroup",
                    "#users",
                }
                scen_found = True

        assert scen_found == True

        response = client.post(
            "/api/scenarios/transport/",
            files={"scenario_data": open("tests/data/transport.miroscen", "rb")},
            data={
                "overwrite_data": True,
                "read_perm": [settings["VALID_AUTH_TUPLE"][0]],
                "exec_perm": [settings["VALID_AUTH_TUPLE"][0]],
            },
            auth=settings["VALID_AUTH_TUPLE"],
        )
        print(response.json())
        assert response.status_code == 201

        scen_names = [
            scen[2]
            for scen in get_scen_metadata("transport")
            if scen[1] == settings["VALID_AUTH_TUPLE"][0]
        ]
        assert "My test" in scen_names

        response = client.get(
            "/api/scenarios/transport/",
            auth=("mirotests_auth_1", "mirotests_auth_11234"),
        )
        scen_metadata = response.json()
        print(scen_metadata)
        assert response.status_code == 200
        assert response.headers["x-total"] == str(len(scen_metadata))
        scen_found = False
        for scenario in scen_metadata:
            if (
                scenario["owner"] == settings["VALID_AUTH_TUPLE"][0]
                and scenario["name"] == "My test"
            ):
                scen_found = True

        assert scen_found == False

        response = client.post(
            "/api/scenarios/transport/",
            files={"scenario_data": open("tests/data/transport.miroscen", "rb")},
            data={
                "overwrite_data": False,
            },
            auth=settings["VALID_AUTH_TUPLE"],
        )
        print(response.json())
        assert response.status_code == 409

        response = client.post(
            "/api/scenarios/transport/",
            files={"scenario_data": open("tests/data/transport.miroscen", "rb")},
            data={
                "overwrite_data": True,
            },
            auth=settings["VALID_AUTH_TUPLE"],
        )
        print(response.json())
        assert response.status_code == 201

        response = client.get(
            "/api/scenarios/transport/", auth=settings["VALID_AUTH_TUPLE"]
        )
        scen_metadata = response.json()
        print(scen_metadata)
        assert response.status_code == 200

        scen_found = False
        for scenario in scen_metadata:
            if (
                scenario["owner"] == settings["VALID_AUTH_TUPLE"][0]
                and scenario["name"] == "My test"
            ):
                assert set(scenario["read_perm"]) == {
                    settings["VALID_AUTH_TUPLE"][0],
                    "#users",
                    "#admins",
                    "#mygroup",
                    "#another_group",
                }
                assert set(scenario["write_perm"]) == {settings["VALID_AUTH_TUPLE"][0]}
                assert set(scenario["exec_perm"]) == {
                    settings["VALID_AUTH_TUPLE"][0],
                    "#users",
                    "#admins",
                    "#mygroup",
                    "#another_group",
                }
                scen_found = True

        assert scen_found == True

        # overwriting locked scenario should throw error
        lock_scenario(
            "transport", "My test", settings["VALID_AUTH_TUPLE"][0], "test123"
        )
        response = client.post(
            "/api/scenarios/transport/",
            files={"scenario_data": open("tests/data/transport.miroscen", "rb")},
            data={
                "read_perm": ["#users", "#mygroup", "#another_group"],
                "overwrite_data": True,
            },
            auth=settings["VALID_AUTH_TUPLE"],
        )
        print(response.json())
        assert response.status_code == 423

        response = client.get(
            "/api/scenarios/transport/", auth=settings["VALID_AUTH_TUPLE"]
        )
        scen_metadata = response.json()
        print(scen_metadata)
        assert response.status_code == 200

        scen_found = False
        for scenario in scen_metadata:
            if (
                scenario["owner"] == settings["VALID_AUTH_TUPLE"][0]
                and scenario["name"] == "My test"
            ):
                assert set(scenario["read_perm"]) == {
                    settings["VALID_AUTH_TUPLE"][0],
                    "#users",
                    "#admins",
                    "#mygroup",
                    "#another_group",
                }
                scen_found = True

        assert scen_found == True

    def test_get_scenario_list(self, cleanup):
        register_transport(client, ["mygroup"])
        invite_user("mirotests_auth_1", 1, group="mygroup")
        response = client.post(
            "/api/scenarios/transport/",
            files={"scenario_data": open("tests/data/transport.miroscen", "rb")},
            data={"overwrite_data": True},
            auth=("mirotests_auth_1", "mirotests_auth_11234"),
        )
        print(response.json())
        assert response.status_code == 201
        response = client.get(
            "/api/scenarios/transport/", auth=settings["VALID_AUTH_TUPLE"]
        )
        scen_metadata = response.json()
        assert len(scen_metadata) == 2
        assert response.headers["x-page"] == "1"
        assert response.headers["x-per-page"] == "20"
        assert scen_metadata[1]["name"] == "My test"
        assert scen_metadata[0]["name"] == "default"
        assert scen_metadata[1]["tags"] == ["tag1", "tag2"]
        assert scen_metadata[0]["tags"] == []
        assert scen_metadata[1]["owner"] == "mirotests_auth_1"
        assert scen_metadata[0]["owner"] == settings["VALID_AUTH_TUPLE"][0]
        assert scen_metadata[1]["last_modified"].startswith(
            datetime.date.today().strftime("%Y-%m-%dT")
        )
        assert scen_metadata[1]["last_modified"].endswith("+00:00") or scen_metadata[1][
            "last_modified"
        ].endswith("Z")
        assert set(scen_metadata[1]["read_perm"]) == {
            "mirotests_auth_1",
            "#mygroup",
            "#users",
        }
        assert set(scen_metadata[0]["read_perm"]) == {
            settings["VALID_AUTH_TUPLE"][0],
            "#mygroup",
            "#another_group",
            "#users",
            "#admins",
        }
        assert scen_metadata[1]["write_perm"] == ["mirotests_auth_1"]
        assert scen_metadata[0]["write_perm"] == [settings["VALID_AUTH_TUPLE"][0]]
        assert set(scen_metadata[1]["exec_perm"]) == {
            "mirotests_auth_1",
            "#mygroup",
            "#users",
        }
        assert set(scen_metadata[0]["exec_perm"]) == {
            settings["VALID_AUTH_TUPLE"][0],
            "#mygroup",
            "#another_group",
            "#users",
            "#admins",
        }

        assert response.status_code == 200

        response = client.get(
            "/api/scenarios/transport/?page=1&per_page=1",
            auth=settings["VALID_AUTH_TUPLE"],
        )
        scen_metadata = response.json()
        assert len(scen_metadata) == 1
        assert response.headers["x-total-pages"] == "2"
        assert response.headers["x-total"] == "2"
        assert response.headers["x-page"] == "1"
        assert response.headers["x-next-page"] == "2"
        assert response.headers["x-prev-page"] == ""
        assert scen_metadata[0]["name"] == "default"
        assert scen_metadata[0]["tags"] == []
        assert scen_metadata[0]["owner"] == settings["VALID_AUTH_TUPLE"][0]

        response = client.get(
            "/api/scenarios/transport/?page=2&per_page=1",
            auth=settings["VALID_AUTH_TUPLE"],
        )
        scen_metadata = response.json()
        assert len(scen_metadata) == 1
        assert response.headers["x-total-pages"] == "2"
        assert response.headers["x-total"] == "2"
        assert response.headers["x-page"] == "2"
        assert response.headers["x-per-page"] == "1"
        assert response.headers["x-next-page"] == ""
        assert response.headers["x-prev-page"] == "1"
        assert scen_metadata[0]["name"] == "My test"
        assert scen_metadata[0]["tags"] == ["tag1", "tag2"]
        assert scen_metadata[0]["owner"] == "mirotests_auth_1"

    def test_download_scenario(self, cleanup):
        register_transport(client, ["mygroup"])
        response = client.get(
            "/api/scenarios/transport/download?name=idontexist",
            auth=settings["VALID_AUTH_TUPLE"],
        )
        assert response.status_code == 404
        response = client.get(
            "/api/scenarios/transport/download?name=default",
            auth=settings["VALID_AUTH_TUPLE"],
        )
        assert response.status_code == 200
        zf = zipfile.ZipFile(io.BytesIO(response.content), "r")
        files_in_zip = []
        metadata_tmp = None
        gdx_content_miroscen = None
        for fileinfo in zf.infolist():
            files_in_zip.append(fileinfo.filename)
            if fileinfo.filename == "metadata.json":
                metadata_tmp = json.loads(zf.read(fileinfo).decode("ascii"))
            elif fileinfo.filename == "data.gdx":
                gdx_content_miroscen = zf.read(fileinfo)

        assert files_in_zip == [
            "metadata.json",
            "data.gdx",
            "views.json",
            "attachments/",
        ]
        assert metadata_tmp["scen_name"] == "default"

        response = client.get(
            "/api/scenarios/transport/download?name=default&file_type=gdx",
            auth=settings["VALID_AUTH_TUPLE"],
        )
        assert response.status_code == 200
        gdx_content_gdx = response.content
        d = response.headers["content-disposition"]
        assert re.findall('filename="(.+)"', d)[0] == "transport_default.gdx"

        assert gdx_content_gdx == gdx_content_miroscen

        response = client.get(
            "/api/scenarios/transport/download?name=default&file_type=csv",
            auth=settings["VALID_AUTH_TUPLE"],
        )
        assert response.status_code == 200
        d = response.headers["content-disposition"]
        assert re.findall('filename="(.+)"', d)[0] == "transport_default.zip"
        zf = zipfile.ZipFile(io.BytesIO(response.content), "r")
        files_in_zip = []
        for fileinfo in zf.infolist():
            files_in_zip.append(fileinfo.filename)

        assert files_in_zip == [
            "_scalars_out.csv",
            "_scalars.csv",
            "a.csv",
            "b.csv",
            "d.csv",
            "ilocdata.csv",
            "jlocdata.csv",
            "schedule.csv",
        ]

        response = client.get(
            "/api/scenarios/transport/download?name=default&file_type=xlsx",
            auth=settings["VALID_AUTH_TUPLE"],
        )
        assert response.status_code == 200
        zf = zipfile.ZipFile(io.BytesIO(response.content), "r")
        files_in_zip = []
        for fileinfo in zf.infolist():
            files_in_zip.append(fileinfo.filename)

        assert "xl/worksheets/sheet9.xml" in files_in_zip

        invite_user("mirotests_auth_1", 1, group="mygroup")
        response = client.post(
            "/api/scenarios/transport/",
            files={"scenario_data": open("tests/data/default.gdx", "rb")},
            data={"overwrite_data": True},
            auth=("mirotests_auth_1", "mirotests_auth_11234"),
        )
        print(response.json())
        assert response.status_code == 201

        response = client.get(
            "/api/scenarios/transport/", auth=settings["VALID_AUTH_TUPLE"]
        )
        scen_metadata = response.json()
        print(scen_metadata)

        response = client.get(
            "/api/scenarios/transport/download?name=default&owner=mirotests_auth_1",
            auth=settings["VALID_AUTH_TUPLE"],
        )
        assert response.status_code == 200

        zf = zipfile.ZipFile(io.BytesIO(response.content), "r")
        files_in_zip = []
        metadata_tmp = None
        for fileinfo in zf.infolist():
            files_in_zip.append(fileinfo.filename)
            if fileinfo.filename == "metadata.json":
                metadata_tmp = json.loads(zf.read(fileinfo).decode("ascii"))

        assert metadata_tmp["uid"] == "mirotests_auth_1"

    def test_delete_scenario(self, cleanup):
        register_transport(client)
        invite_user("mirotests_auth_1", 1, group="mygroup")
        response = client.post(
            "/api/scenarios/transport/",
            files={"scenario_data": open("tests/data/transport.miroscen", "rb")},
            data={
                "overwrite_data": True,
            },
            auth=settings["VALID_AUTH_TUPLE"],
        )
        print(response.json())
        assert response.status_code == 201

        response = client.delete(
            "/api/scenarios/transport/?name=idontexist",
            auth=settings["VALID_AUTH_TUPLE"],
        )
        print(response.json())
        assert response.status_code == 404

        response = client.delete(
            "/api/scenarios/transport/?name=My%20test",
            auth=settings["VALID_AUTH_TUPLE"],
        )
        print(response.json())
        assert response.status_code == 200

        response = client.delete(
            "/api/scenarios/transport/?name=My%20test",
            auth=settings["VALID_AUTH_TUPLE"],
        )
        print(response.json())
        assert response.status_code == 404

        response = client.post(
            "/api/scenarios/transport/",
            files={"scenario_data": open("tests/data/transport.miroscen", "rb")},
            data={
                "overwrite_data": True,
                "read_perm": ["mirotests_auth_1"],
                "write_perm": ["mirotests_auth_1"],
                "execPerm": ["mirotests_auth_1"],
            },
            auth=("mirotests_auth_1", "mirotests_auth_11234"),
        )
        print(response.json())
        assert response.status_code == 201

        response = client.delete(
            "/api/scenarios/transport/?name=My%20test&owner=mirotests_auth_1",
            auth=settings["VALID_AUTH_TUPLE"],
        )
        print(response.json())
        assert response.status_code == 404

        response = client.delete(
            "/api/scenarios/transport/?name=My%20test&owner=mirotests_auth_1",
            auth=("mirotests_auth_1", "mirotests_auth_11234"),
        )
        print(response.json())
        assert response.status_code == 200

        response = client.delete(
            "/api/scenarios/transport/?name=My%20test&owner=mirotests_auth_1",
            auth=("mirotests_auth_1", "mirotests_auth_11234"),
        )
        print(response.json())
        assert response.status_code == 404

        response = client.post(
            "/api/scenarios/transport/",
            files={"scenario_data": open("tests/data/transport.miroscen", "rb")},
            data={
                "overwrite_data": True,
                "read_perm": ["mirotests_auth_1", "#mygroup"],
                "write_perm": ["mirotests_auth_1", "#mygroup"],
                "execPerm": ["mirotests_auth_1", "#mygroup"],
            },
            auth=("mirotests_auth_1", "mirotests_auth_11234"),
        )
        print(response.json())
        assert response.status_code == 201

        response = client.delete(
            "/api/scenarios/transport/?name=My%20test&owner=mirotests_auth_1",
            auth=settings["VALID_AUTH_TUPLE"],
        )
        print(response.json())
        assert response.status_code == 200

        response = client.post(
            "/api/scenarios/transport/",
            files={"scenario_data": open("tests/data/transport.miroscen", "rb")},
            data={
                "overwrite_data": True,
            },
            auth=settings["VALID_AUTH_TUPLE"],
        )
        print(response.json())
        assert response.status_code == 201

        # deleting locked scenario should throw error
        lock_scenario(
            "transport", "My test", settings["VALID_AUTH_TUPLE"][0], "test123"
        )
        response = client.delete(
            "/api/scenarios/transport/?name=My%20test",
            auth=settings["VALID_AUTH_TUPLE"],
        )
        print(response.json())
        assert response.status_code == 423
