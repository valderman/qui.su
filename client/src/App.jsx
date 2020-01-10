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
import Header         from './Header.jsx';
import StorageManager from './StorageManager.js';
import { googleSignOut } from './GoogleSignIn.jsx';
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
    onLogout = gapi => {
        googleSignOut(() =>  {
            api.unsetAuthToken();
            storageManager.remove('loggedInUser');
            setUser(null);
        });
    };
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
                            <div>
                                <Header
                                    api={api}
                                    user={renderUser}
                                    onSignOut={onLogout}
                                />
                                {user ? <Presenter
                                            id={p.match.params.id}
                                               api={api}
                                />
                                 : <Redirect to="/" />
                                }
                            </div>
                        }
                    />
                    <Route
                        path="/:id"
                        render={p => <Deck url={p.match.params.id} api={api} />}
                    />
                    <Route path="/">
                        <Header
                            api={api}
                            user={renderUser}
                            onSignOut={onLogout}
                        />
                        { renderUser
                          ? <HomePage
                                api={api}
                                onLogout={onLogout}
                                user={renderUser}
                          />
                          : <LandingPage
                                api={api}
                                error={error}
                                onLogin={onLogin}
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
