import React from 'react';
import './css/GoogleSignIn.css';

class GoogleSignIn extends React.Component {
    constructor(props) {
        super(props);
        this.api = props.api;
        this.clientId = props.clientId;
    }

    async onSuccess(googleUser) {
        const result = await this.api.authenticate(googleUser.getAuthResponse().id_token);
        if(result && this.props.onSuccess instanceof Function) {
            this.props.onSuccess(result);
        } else if(this.props.onFailure instanceof Function) {
            this.props.onFailure();
        }
    }

    onFailure() {
        if(this.props.onFailure instanceof Function) {
            this.props.onFailure();
        }
    }

    initSignInButton() {
        window.gapi.load('auth2', () => {
            this.auth2 = window.gapi.auth2.init({
                client_id: this.clientId
            });
        });

        window.gapi.load('signin2', () => {
            window.gapi.signin2.render('loginButton', {
                width: 300,
                height: 50,
                longtitle: 'true',
                theme: 'dark',
                onsuccess: this.onSuccess.bind(this),
                onfailure: this.onFailure.bind(this)
            });
        });
    }

    componentDidMount() {
        const script = document.createElement('SCRIPT');
        script.src = 'https://apis.google.com/js/platform.js';
        script.async = true;
        script.defer = true;
        script.onload = this.initSignInButton.bind(this);
        document.head.appendChild(script);
    }

    render() {
        return (
            <span id="googleSignIn">
                <span id="loginButton"/>
            </span>
        );
    }
}
export default GoogleSignIn;
