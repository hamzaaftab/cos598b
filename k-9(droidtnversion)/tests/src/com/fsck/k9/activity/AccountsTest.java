package com.fsck.k9droidtn.activity;

import android.test.ActivityInstrumentationTestCase2;
import com.fsck.k9droidtn.activity.Accounts;
/**
 * This is a simple framework for a test of an Application.  See
 * {@link android.test.ApplicationTestCase ApplicationTestCase} for more information on
 * how to write and extend Application tests.
 * <p/>
 * To run this test, you can type:
 * adb shell am instrument -w \
 * -e class com.fsck.k9droidtn.activity.AccountsTest \
 * com.fsck.k9droidtn.tests/android.test.InstrumentationTestRunner
 */
public class AccountsTest extends ActivityInstrumentationTestCase2<Accounts> {

    public AccountsTest() {
        super("com.fsck.k9droidtn", Accounts.class);
    }

}
