class StorageManager {
    constructor(managerKey, defaultLifetime, store) {
        this.managerKey = managerKey;
        this.defaultLifetime = defaultLifetime || (3600*1000);
        this.store = store || window.localStorage;
    }

    read(key, defaultValue = null) {
        const text = this.store[`${this.managerKey}:${key}`];
        if(!text) {
            return defaultValue;
        }
        try {
            const data = JSON.parse(text);
            if(data.expires < new Date().getTime()) {
                this.remove(key);
                return defaultValue;
            }
            return data.data;
        } catch(e) {
            this.remove(key);
            return defaultValue;
        }
    }

    write(key, value, lifetime) {
        const ttl = lifetime || this.defaultLifetime;
        const data = {
            data: value,
            expires: new Date().getTime() + ttl
        };
        this.store[`${this.managerKey}:${key}`] = JSON.stringify(data);
        return value;
    }

    remove(key) {
        delete this.store[`${this.managerKey}:${key}`];
    }
}

export default StorageManager;
