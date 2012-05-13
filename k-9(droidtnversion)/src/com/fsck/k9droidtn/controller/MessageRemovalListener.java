package com.fsck.k9droidtn.controller;

import com.fsck.k9droidtn.mail.Message;

public interface MessageRemovalListener {
    public void messageRemoved(Message message);
}
