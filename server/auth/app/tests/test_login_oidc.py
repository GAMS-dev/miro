import os
import shutil
import pytest
import requests
from unittest.mock import patch, MagicMock
from fastapi.testclient import TestClient

from ..main import app
from ..config import settings as app_settings
from .util import settings, get_db_cursor, register_transport, invite_user, delete_user, reset_app_config_file

client = TestClient(app)


@pytest.fixture()
def cleanup():
    app_settings.authentication_mode = "oidc"
    requests.post(f"{settings['ENGINE_URL']}/namespaces/{settings['ENGINE_NS']}/user-groups?label=mygroup",
                  auth=settings["VALID_AUTH_TUPLE"])
    os.remove(settings["SPECS_FILE_PATH"])
    reset_app_config_file()
    yield
    conn, cur = get_db_cursor()
    for app_id in ["transport", "transport_test"]:
        cur.execute(f'DROP SCHEMA IF EXISTS "M_{app_id.upper()}" CASCADE')
        requests.delete(f"{settings['ENGINE_URL']}/namespaces/{settings['ENGINE_NS']}/models/{app_id}",
                        auth=settings["VALID_AUTH_TUPLE"])
        try:
            shutil.rmtree(f'/home/miro/admin/models/{app_id}')
        except FileNotFoundError:
            pass
        try:
            shutil.rmtree(f'/home/miro/admin/data/data_{app_id}')
        except FileNotFoundError:
            pass
    conn.commit()
    cur.close()
    conn.close()
    reset_app_config_file()
    for group_label in ["mygroup", "Mygroup"]:
        requests.delete(f"{settings['ENGINE_URL']}/namespaces/{settings['ENGINE_NS']}/user-groups?label={group_label}",
                        auth=settings["VALID_AUTH_TUPLE"])
    delete_user("mirotests_auth_1", allow_fail=True)


def create_mock_login(username, password):
    mock_response = MagicMock()
    r = requests.post(f"{settings['ENGINE_URL']}/auth/login",
                      data={"expires_in": 100,
                            "username": username,
                            "password": password})
    mock_response.json = lambda: {"token": r.json()["token"]}
    mock_response.status_code = 200
    return mock_response


class TestOidc:
    @patch('requests.post')
    def test_login_invalid(self, mock_login, cleanup):
        mock_response = MagicMock()
        mock_response.json.token = "Unauthorized"
        mock_response.status_code = 401
        mock_login.return_value = mock_response

        response = client.post("/login",
                               json={"username": settings["ENGINE_USER"],
                                     "password": settings["ENGINE_PASSWORD"]})
        print(response.json())
        assert response.status_code == 400

        response = client.post("/login/oidc",
                               json={"id_token": "invalid"})
        print(response.json())
        assert response.status_code == 401
        response = client.post("/login/oidc",
                               json={"username": settings["ENGINE_USER"],
                                     "password": settings["ENGINE_PASSWORD"]})
        print(response.json())
        assert response.status_code == 422

    def test_login_no_perm(self, cleanup):
        invite_user("mirotests_auth_1", 0)

        with patch("requests.post", return_value=create_mock_login(
                "mirotests_auth_1", "mirotests_auth_1")):
            response = client.post("/login/oidc",
                                   json={"id_token": "valid"})
        print(response.json())
        assert response.status_code == 403
        response = requests.post(f"{settings['ENGINE_URL']}/namespaces/{settings['ENGINE_NS']}/user-groups/mygroup",
                                 data={"username": "mirotests_auth_1"},
                                 auth=settings["VALID_AUTH_TUPLE"])
        print(response.json())
        assert response.status_code == 201
        # user without permissions but member of group should get access only if models exist
        with patch("requests.post", return_value=create_mock_login(
                "mirotests_auth_1", "mirotests_auth_1")):
            response = client.post("/login/oidc",
                                   json={"id_token": "valid"})
        print(response.json())
        assert response.status_code == 403

        register_transport(client, ["mygroup"])
        with patch("requests.post", return_value=create_mock_login(
                "mirotests_auth_1", "mirotests_auth_1")):
            response = client.post("/login/oidc",
                                   json={"id_token": "valid"})
        print(response.json())
        assert response.status_code == 200

    def test_login_with_perm(self, cleanup):
        invite_user("mirotests_auth_1", 1)

        with patch("requests.post", return_value=create_mock_login(
                "mirotests_auth_1", "mirotests_auth_1")):
            response = client.post("/login/oidc",
                                   json={"id_token": "valid"})
        print(response.json())
        assert response.status_code == 200
        with patch("requests.post", return_value=create_mock_login(
                settings["ENGINE_USER"], settings["ENGINE_PASSWORD"])):
            response = client.post("/login/oidc",
                                   json={"id_token": "valid"})
        print(response.json())
        assert response.status_code == 200

    def test_user_groups(self, cleanup):
        # groups with uppercase letters in labels should not be accepted by MIRO Server
        invite_user("mirotests_auth_1", 7, "mygroup", inviter=True)
        response = requests.post(f"{settings['ENGINE_URL']}/namespaces/{settings['ENGINE_NS']}/user-groups?label=Mygroup",
                                 auth=("mirotests_auth_1", "mirotests_auth_1"))
        print(response.json())
        assert response.status_code == 201

        with patch("requests.post", return_value=create_mock_login(
                "mirotests_auth_1", "mirotests_auth_1")):
            response = client.post("/login/oidc",
                                   json={"id_token": "valid"})

        response_data = response.json()
        print(response_data)
        assert response_data["roles"] == ['mygroup', 'users', 'admins']
        assert response.status_code == 200
