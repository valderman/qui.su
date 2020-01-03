class StorageManager {
    constructor(managerKey, defaultLifetime, store) {
        this.managerKey = managerKey;
        this.defaultLifetime = defaultLifetime || (3600*1000);
        this.store = store || window.localStorage;
    }

    readWithLifetime(key, defaultValue = null) {
        const text = this.store[`${this.managerKey}:${key}`];
        if(!text) {
            return [defaultValue, undefined];
        }
        try {
            const data = JSON.parse(text);
            const ttl = data.expires - new Date().getTime();
            if(ttl <= 0) {
                this.remove(key);
                return [defaultValue, ttl];
            }
            return [data.data, ttl];
        } catch(e) {
            this.remove(key);
            return [defaultValue, undefined];
        }
    }

    read(key, defaultValue = null) {
        return this.readWithLifetime(key, defaultValue)[0];
    }

    getExpiryTime(key) {
        const text = this.store[`${this.managerKey}:${key}`];
        try {
            return JSON.parse(text).expires;
        } catch(e) {
            return undefined;
        }
    }

    getRemainingLifetime(key) {
        const expires = this.getExpiryTime(key);
        return expires - new Date().getTime();
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
