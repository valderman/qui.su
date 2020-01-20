import React        from 'react';
import GoogleSignIn from './GoogleSignIn.jsx';
import Settings     from './Settings.js';
import './css/LandingPage.css';

function LandingPage(props) {
    return (
        <main className="landingPage">
            <h1>This is a quiz site!</h1>
            <p>
                If you're a teacher looking to make your lectures more
                interactive, or you just want to run a quiz for fun,
                we've got your back. Or at least we will.
                Soonish. We promise. Really.
            </p>
            <p>
                <GoogleSignIn
                    api={props.api}
                    clientId={Settings.GOOGLE_CLIENT_ID}
                    onSuccess={props.onLogin}
                    onFailure={props.onLoginFail}
                />
            </p>
            {props.error && <p className="error">{props.error}</p>}
        </main>
    );
}

export default LandingPage;
