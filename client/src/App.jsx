import React, { useState }
                   from 'react';
import { Switch, Route, Redirect, BrowserRouter as Router }
                   from 'react-router-dom';
import Deck        from './Deck.jsx';
import Presenter   from './Presenter.jsx';
import QuizApi     from './QuizApi.js';
import LandingPage from './LandingPage.jsx';
import HomePage    from './HomePage.jsx';
import Settings    from './Settings.js';
import './css/App.css';
import './css/theme.css';
import './css/fontawesome.css';
import 'highlight.js/styles/github.css';

function App() {
    const [user, setUser] = useState(null);
    const [error, setError] = useState(null);
    let api;
    const onLogout = async () => {
        const auth2 = window.gapi.auth2.getAuthInstance();
        await auth2.signOut();
        api.unsetAuthToken();
        setUser(null);
    }
    api = new QuizApi(Settings.API_PREFIX, async () => {
        await onLogout();
        setError("Your session has expired, please log in again.");
    });
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
                        { user ? <HomePage api={api} onLogout={onLogout} user={user} />
                               : <LandingPage api={api} error={error} onLogin={setUser} />
                        }
                    </Route>
                </Switch>
            </Router>
        </div>
    );
}

export default App;
