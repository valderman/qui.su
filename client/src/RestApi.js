const LONGPOLL_ERROR_THRESHOLD = 2;

function getTimestamp() {
    return new Date().getTime()/1000;
}

class RestException {
    constructor(result) {
        this.status = result.status;
    }
}

class SessionExpiredException {
}

class RestApi {
    constructor(baseUrl, storageManager, onExpire) {
        this.baseUrl = baseUrl || "";
        this.storageManager = storageManager;
        this.onExpire = (onExpire || (() => undefined)).bind(null);
    }

    url(parts) {
        return this.baseUrl + '/' + parts.join('/');
    }

    setAuthHeader(init) {
        const [token, ttl] = this.storageManager.readWithLifetime('restApiAuthToken');
        if(ttl <= 0) {
            this.storageManager.remove('restApiAuthToken');
            return init;
        }
        if(token) {
            if(!init) {
                init = {};
            }
            if(!init.headers) {
                init.headers = {};
            }
            init.headers.Authentication = 'Bearer ' + token;
        }
        return init;
    }

    async get(parts) {
        let init = this.setAuthHeader();
        const result = await window.fetch(this.url(parts), init);
        return await this.checkResult(result);
    }

    async post(parts, body, contentType) {
        const ctype = contentType || 'application/json';
        const json = ctype === 'application/json';
        let init = {
            method: 'POST',
            body: json ? JSON.stringify(body) : body,
            headers: {'Content-Type': ctype}
        };
        this.setAuthHeader(init);
        const result = await window.fetch(this.url(parts), init);
        return await this.checkResult(result);
    }

    async checkResult(result) {
        switch(result.status) {
        case 403:
        case 401:
            this.onExpire();
            throw new SessionExpiredException();
        case 200:
            return await result.json();
        default:
            throw new RestException(result);
        }
    }

    async longPoll(parts) {
        let init = {
            headers: {'Cache-Control': 'no-cache'}
        };
        this.setAuthHeader(init);
        while(true) {
            let t0 = getTimestamp();
            try {
                return await this.get(parts, init);
            } catch (e) {
                let t1 = getTimestamp();
                if(t1 - t0 < LONGPOLL_ERROR_THRESHOLD) {
                    throw 'long poll dying at an alarming rate: ' + JSON.stringify(e);
                }
            }
        }
    }

    setAuthToken(token, expires) {
        this.storageManager.write('restApiAuthToken', token, (expires - getTimestamp())*1000);
    }

    unsetAuthToken() {
        this.storageManager.remove('restApiAuthToken');
    }
}
export default RestApi;
