'use strict'

const Database = require('better-sqlite3');

class MiroDb {
    constructor (dbPath) {
        this.db = new Database(dbPath);
    }
    getOrphans (appNames) {
        const data = this._getAllTables();
        return data.map((row) => row.name).filter((tableName) =>
            appNames.filter((appName) =>
                this._tableBelongsToApp(tableName, appName)).length === 0
        );
    }
    removeAppDbTables (appName) {
        const data = this._getAllTables();
        const tablesToRemove = data.map((row) => row.name).filter((tableName) =>
            this._tableBelongsToApp(tableName, appName)
        );
        return this.removeTables(tablesToRemove)
    }
    removeTables (tablesToRemove) {
        try {
            this.db.pragma('foreign_keys = OFF');
            for ( const tableToRemove of tablesToRemove ) {
                const stmt = this.db.prepare(`DROP TABLE IF EXISTS ${this._escapeIdentifier(tableToRemove)}`);
                stmt.run();
            }
        } catch (error) {
            throw error;
        } finally {
            this.db.pragma('foreign_keys = ON');
        }
        return this;
    }
    appTablesExist (appName) {
        const data = this._getAllTables();
        return data.map((row) => row.name).filter((tableName) => 
            this._tableBelongsToApp(tableName, appName)).length > 0;
    }
    close () {
        this.db.close();
    }
    _getAllTables () {
        const stmt = this.db.prepare(`SELECT name FROM sqlite_master WHERE type = 'table'`);
        return stmt.all();
    }
    _escapeIdentifier (identifier) {
        return '`' + identifier.replace(/`/g, '``') + '`'
    }
    _tableBelongsToApp (tableName, appName) {
        return tableName.startsWith(`${MiroDb.escapeAppId(appName)}_`) || 
                (tableName.startsWith('_sys_') && tableName.endsWith(`_${appName.toLowerCase()}`));
    }
    static escapeAppId (appId) {
        return appId.toLowerCase().replace(/_/g, '')
    }
}

module.exports = MiroDb;
