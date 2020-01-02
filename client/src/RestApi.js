const LONGPOLL_ERROR_THRESHOLD = 2;

function getTimestamp() {
    return new Date().getTime()/1000;
}

class RestApi {
    constructor(baseUrl, onExpire) {
        this.baseUrl = baseUrl || "";
        this.onExpire = onExpire || (() => undefined);
        const authToken = window.localStorage.getItem('restApiAuthToken');
        const expires = window.localStorage.getItem('restApiAuthTokenExpires');
        if(expires > getTimestamp()) {
            this.setAuthToken(authToken, expires);
        }
    }

    url(parts) {
        return this.baseUrl + '/' + parts.join('/');
    }

    setAuthHeader(init) {
        if(!init) {
            init = {};
        }
        if(this.authToken) {
            if(this.expires && this.expires <= getTimestamp()) {
                const f = this.onExpire;
                f();
            } else {
                if(!init.headers) {
                    init.headers = {};
                }
                init.headers.Authentication = 'Bearer ' + this.authToken;
            }
        }
        return init;
    }

    async get(parts) {
        let init = this.setAuthHeader();
        const result = await window.fetch(this.url(parts), init);
        if(result.status === 403) {
            this.onExpire();
            throw '403';
        } else {
            return await result.json();
        }
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
        if(result.status === 403) {
            this.onExpire();
            throw '403';
        } else {
            return await result.json();
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
        this.authToken = token;
        this.expires = expires;
        window.localStorage.setItem('restApiAuthToken', token);
        window.localStorage.setItem('restApiAuthTokenExpires', expires);
    }

    unsetAuthToken() {
        window.localStorage.removeItem('restApiAuthToken');
        window.localStorage.removeItem('restApiAuthTokenExpires');
        delete this.authToken;
        delete this.expires;
    }
}
export default RestApi;
