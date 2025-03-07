from datetime import datetime
import os
import requests
import psycopg2

settings = {
    "ENGINE_URL": os.environ["ENGINE_URL"],
    "ENGINE_NS": os.environ["ENGINE_NS"],
    "ENGINE_USER": os.environ["ENGINE_USER"],
    "ENGINE_PASSWORD": os.environ["ENGINE_PASSWORD"],
    "VALID_AUTH_TUPLE": (os.environ["ENGINE_USER"], os.environ["ENGINE_PASSWORD"]),
    "SPECS_FILE_PATH": "/home/miro/admin/data/specs.yaml",
}


def reset_app_config_file():
    with open(settings["SPECS_FILE_PATH"], "w") as f:
        f.write(
            "specs:\n- accessGroups:\n  - admins\n  containerEnv:\n    MIRO_API_VERSION: '1'\n  containerVolumes:\n  - :/home/miro/admin/models\n  - :/home/miro/admin/data\n  description: MIRO Server Admin Panel\n  displayName: Admin\n  id: admin\n  logoURL: ~\n"
        )


def get_db_cursor():
    conn = psycopg2.connect(
        dbname=os.environ["GMS_MIRO_DATABASE"],
        user=os.environ["GMS_MIRO_DATABASE_USER"],
        host=os.environ["GMS_MIRO_DATABASE_HOST"],
        password=os.environ["GMS_MIRO_DATABASE_PWD"],
        port=5432,
    )
    return conn, conn.cursor()


def lock_scenario(app_id: str, scen_name: str, owner: str, lock_user: str) -> None:
    conn, cur = get_db_cursor()
    cur.execute(
        f'SELECT * FROM "M_{app_id.upper()}"."_sys_metadata_" WHERE "_sname"=\'{scen_name}\' AND "_uid"=\'{owner}\''
    )
    scen_data = cur.fetchall()
    cur.execute(
        f"""
    INSERT INTO "M_{app_id.upper()}"."_sys_scenlocks_" VALUES (%s, %s, %s);
""",
        (str(lock_user), int(scen_data[0][0]), datetime.today()),
    )
    conn.commit()
    cur.close()
    conn.close()


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


def register_transport(client, access_groups=None):
    with open("tests/data/transport.miroapp", "rb") as app_data:
        response = client.post(
            "/api/apps/",
            files={"app_data": app_data},
            data={
                "overwrite_data": True,
                "access_groups": access_groups if access_groups else [],
            },
            auth=settings["VALID_AUTH_TUPLE"],
        )
        print(response.json())
        assert response.status_code == 201


def invite_user(name, permissions, group=None, inviter=False):
    payload = {}
    if permissions:
        payload["namespace_permissions"] = f"{permissions}@{settings['ENGINE_NS']}"
    if group:
        payload["user_groups"] = f"{group}@{settings['ENGINE_NS']}"
    if inviter:
        payload["roles"] = "inviter"

    response = requests.post(
        f"{settings['ENGINE_URL']}/users/invitation",
        data=payload,
        auth=settings["VALID_AUTH_TUPLE"],
    )
    print(response.json())
    assert response.status_code == 201
    invitation_code = response.json()["invitation_token"]
    response = requests.post(
        f"{settings['ENGINE_URL']}/users/",
        data={
            "username": name,
            "password": f"{name}1234",
            "invitation_code": invitation_code,
        },
        auth=settings["VALID_AUTH_TUPLE"],
    )
    print(response.json())
    assert response.status_code == 201


def delete_user(name, allow_fail=False):
    response = requests.delete(
        f"{settings['ENGINE_URL']}/users/?username={name}&delete_results=true&delete_children=true",
        auth=settings["VALID_AUTH_TUPLE"],
    )
    print(response.json())
    if not allow_fail:
        assert response.status_code == 200
