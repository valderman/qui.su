import React, { useState, useEffect } from 'react';
import List from './List.jsx';

function UserAdminPanel(props) {
    const [users, setUsers] = useState([]);
    const renderUsers = users.map(u => ({
        key: u.userId,
        name: u.userName + " (" + u.userEmail + ")"
    }));
    const buttons = [{
        title: 'Make admin',
        className: 'fa-crown',
        onClick: () => alert('TODO: admin management')
    }, {
        title: 'Ban user',
        className: 'fa-gavel',
        onClick: () => alert('TODO: implement ban/unban')
    }
    ];
    useEffect(() => {
        props.api.findUsers('', 100000).then(setUsers);
    }, []);
    return (
        <div className="userAdminPanel">
            <h3>Users</h3>
            <List items={renderUsers} buttons={buttons} />
        </div>
    );
}

export default UserAdminPanel;
