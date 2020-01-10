import React from 'react';
import Settings from './Settings.js';
import './css/GoogleSignIn.css';

const NO_CROSS_SITE_COOKIES = `You need to enable cross-site cookies for this site to sign in.`;
const LOGIN_FAILED = `We were unable to sign you in.
Are you sure you have cross-site cookies enabled for this site?`;

class GoogleSignIn extends React.Component {
    constructor(props) {
        super(props);
        this.api = props.api;
        this.clientId = props.clientId;
        this.onSuccess = (this.props.onSuccess || (() => undefined)).bind(null);
        this.onFailure = (this.props.onFailure || (() => undefined)).bind(null);
    }

    async success(googleUser) {
        const result = await this.api.authenticate(googleUser.getAuthResponse().id_token);
        if(result) {
            this.onSuccess(result);
        } else {
            this.onFailure(LOGIN_FAILED);
        }
    }

    initSignInButton(gapi) {
        window.gapi.signin2.render('loginButton', {
            width: 300,
            height: 50,
            longtitle: 'true',
            theme: 'dark',
            onsuccess: this.success.bind(this),
            onfailure: () => this.onFailure(LOGIN_FAILED)
        });
    }

    componentDidMount() {
        withGoogleApi(
            this.initSignInButton.bind(this),
            () => this.onFailure(NO_CROSS_SITE_COOKIES)
        );
    }

    render() {
        return (
            <span id="googleSignIn">
                <span id="loginButton"/>
            </span>
        );
    }
}

function withGoogleApi(f, fail) {
    if(!window.gapi) {
        const script = document.createElement('SCRIPT');
        script.src = 'https://apis.google.com/js/platform.js';
        script.async = true;
        script.defer = true;
        script.onload = () => {
            window.gapi.load('auth2', async () => {
                try {
                    await window.gapi.auth2.init({
                        client_id: Settings.GOOGLE_CLIENT_ID
                    });
                } catch (e) {
                    if(fail) {
                        fail();
                    } else {
                        throw e;
                    }
                }
                window.gapi.load('signin2', async () => {
                    f(window.gapi);
                });
            });
        };
        document.head.appendChild(script);
    } else {
        f(window.gapi);
    }
}

function googleSignOut(onSignOut) {
    withGoogleApi(async gapi => {
        const auth2 = window.gapi.auth2.getAuthInstance();
        await auth2.signOut();
        onSignOut();
    });
}

export default GoogleSignIn;
export { googleSignOut };
