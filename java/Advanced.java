import java.lang.reflect.Method;
import java.lang.reflect.Constructor;
import java.lang.reflect.Field;

import java.lang.annotation.Annotation;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;


public class Advanced
{
    public static void main(String[] args)
    {
        System.out.println("Starting Advanced");
        Advanced adv = new Advanced();
        adv.reflection();
    }

    private void reflection() {
        System.out.println("Starting Reflection API");
        final Method[] methods = String.class.getMethods();

        System.out.println("String Methods:");
        for( final Method method: methods) {
            System.out.println(" - " + method.getName());
        }

        System.out.println("Create String through reflection:");
        try {
            final Constructor< String > constructor = String.class.getConstructor( String.class );
            final String str = constructor.newInstance( "sample string" );
            final Method method = String.class.getMethod( "length" );
            final int length = (int)method.invoke( str );
            assert length == 13;
            System.out.println("Created string: \"" + str + "\" with length=" + length);
        } catch (Exception e) {
            System.out.println("FAIL: " + e);
        }

        {
            System.out.println("Read annotations");
            ExampleAnnotation annotation =
                ExampleAnnotated.class.getAnnotation(
                    ExampleAnnotation.class
                );

            assert annotation != null;
            System.out.println("annotation = " + annotation);
        }

        {
            System.out.println("Violating privacy");
            final PrivateField priv = new PrivateField();
            try {
                final Field field = PrivateField.class.getDeclaredField( "name" );
                field.setAccessible( true );
                field.set( priv, "violated-privacy" );
            } catch (Exception e) {
                System.out.println("FAIL: privacy");
            }
            System.out.println("New name: " + priv.name);
        }

    }

    @Retention( RetentionPolicy.RUNTIME )
    @Target( ElementType.TYPE )
    public @interface ExampleAnnotation {
        // Some attributes here
    }

    @ExampleAnnotation
    public class ExampleAnnotated {
        // some getters and setters
    }

    /**
     * External can mutate `name` using reflection.
     */
    public class PrivateField {

        private String name;

        public String getName() {
            return name;
        }
    }


}
