package util;

public final class Assert {

    private Assert() {
    }

    public static void notNull(ConcurrentReferenceHashMap.ReferenceType referenceType, String s) {
        assert referenceType != null : s;
    }

    public static void state(boolean b, String noEntriesSegment) {
        assert b : noEntriesSegment;
    }

    public static void isTrue(boolean b, String s) {
        assert b : s;
    }
}
