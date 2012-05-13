
package com.fsck.k9droidtn.mail;

public class CertificateValidationException extends MessagingException {
    public static final long serialVersionUID = -1;

    public CertificateValidationException(String message) {
        super(message);
    }

    public CertificateValidationException(String message, Throwable throwable) {
        super(message, throwable);
    }
}