import React from 'react';
import UserAdminPanel from './UserAdminPanel.jsx';

function AdminPanel(props) {
    return (
        <div className="adminPanel">
            <h2>Admin Panel</h2>
            Admin-only panel for creating invite codes, swinging the banhammer,
            etc.
            <div className="widgets">
                <UserAdminPanel api={props.api} />
            </div>
        </div>
    );
}

export default AdminPanel;
