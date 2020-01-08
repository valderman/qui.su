import React, { useState }
                      from 'react';
import { Switch, Route, Redirect, BrowserRouter as Router }
                      from 'react-router-dom';
import Deck           from './Deck.jsx';
import Presenter      from './Presenter.jsx';
import QuizApi        from './QuizApi.js';
import LandingPage    from './LandingPage.jsx';
import HomePage       from './HomePage.jsx';
import Settings       from './Settings.js';
import StorageManager from './StorageManager.js';
import './css/App.css';
import './css/theme.css';
import './css/fontawesome.css';
import 'highlight.js/styles/github.css';

let onLogout, onError;
const storageManager = new StorageManager('app', 8*3600*1000);
const api = new QuizApi(Settings.API_PREFIX, storageManager, async () => {
    await onLogout();
    onError("Your session has expired, please log in again.");
});

function App() {
    const [user, setUser] = useState(null);
    const [error, setError] = useState(null);
    onError = setError;
    onLogout = () => {
        const script = document.createElement('SCRIPT');
        script.src = 'https://apis.google.com/js/platform.js';
        script.async = true;
        script.defer = true;
        script.onload = () => {
            window.gapi.load('auth2', async () => {
                await window.gapi.auth2.init({
                    client_id: Settings.GOOGLE_CLIENT_ID
                });
                window.gapi.load('signin2', async () => {
                    const auth2 = window.gapi.auth2.getAuthInstance();
                    await auth2.signOut();
                    api.unsetAuthToken();
                    storageManager.remove('loggedInUser');
                    setUser(null);
                });
            });
        };
        document.head.appendChild(script);
    }
    const onLogin = u => {
        setUser(u);
        storageManager.write('loggedInUser', u);
    };
    const storedUser = storageManager.read('loggedInUser');
    let renderUser = user;
    if(!user && storedUser) {
        renderUser = storedUser;
        onLogin(storedUser);
    }
    return (
        <div className="App">
            <Router>
                <Switch>
                    <Route
                        path="/overhead/:id"
                        render={p =>
                            <Deck
                                id={p.match.params.id}
                                api={api}
                                overhead="true"
                            />
                        }
                    />
                    <Route
                        path="/present/:id"
                        render={p =>
                            user ? <Presenter
                                       id={p.match.params.id}
                                       api={api}
                                   />
                                 : <Redirect to="/" />
                        }
                    />
                    <Route
                        path="/:id"
                        render={p => <Deck url={p.match.params.id} api={api} />}
                    />
                    <Route path="/">
                        { renderUser
                              ? <HomePage
                                    api={api}
                                    onLogout={onLogout}
                                    user={renderUser}
                                />
                              : <LandingPage
                                    api={api}
                                    error={error}
                                    nLogin={onLogin}
                                    onLoginFail={setError}
                                />
                        }
                    </Route>
                </Switch>
            </Router>
        </div>
    );
}

export default App;
