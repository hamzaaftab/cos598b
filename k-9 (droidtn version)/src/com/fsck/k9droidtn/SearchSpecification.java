
package com.fsck.k9droidtn;

import com.fsck.k9droidtn.R;
import com.fsck.k9droidtn.mail.Flag;

public interface SearchSpecification {

    public Flag[] getRequiredFlags();

    public Flag[] getForbiddenFlags();

    public boolean isIntegrate();

    public String getQuery();

    public String[] getAccountUuids();

    public String[] getFolderNames();
}