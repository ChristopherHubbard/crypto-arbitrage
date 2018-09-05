import * as React from 'react';
import { render } from 'react-dom';
import { Provider } from 'react-redux';
import { App } from './components';
import { createStore, combineReducers } from 'redux';
import { authentication, registration } from './reducers';

// Combine the reducers into a top level reducer
const rootReducer = combineReducers({
    authentication,
    registration
});

// Create the store using the combined reducers
const store = createStore(rootReducer);

// Render the top level app component -- provide the store
render(
    <Provider store={store}>
        <App/>
    </Provider>,
    document.getElementById('app')
);