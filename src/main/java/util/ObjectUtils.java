package util;

import java.util.Objects;

public final class ObjectUtils {

    private ObjectUtils() {
    }

    public static <V> boolean nullSafeEquals(V value, V oldValue) {
        return Objects.equals(value, oldValue);
    }

    public static <K> int nullSafeHashCode(K key) {
        return Objects.hashCode(key);
    }
}
