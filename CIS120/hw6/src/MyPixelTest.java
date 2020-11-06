import org.junit.Test;
import static org.junit.Assert.*;


/** 
 *  Use this file to test your implementation of Pixel.
 * 
 *  We will manually grade this file to give you feedback
 *  about the completeness of your test cases.
 */

public class MyPixelTest {

    @Test
    public void createPixelTest() {
        Pixel p = new Pixel(1, 256, 3);
        
        assertEquals(1, p.getRed());
        assertEquals(255, p.getGreen());
        assertEquals(3, p.getBlue());
    }
    
    @Test
    public void negativeVals() {
        Pixel p = new Pixel(-1, -256, -3);
        
        assertEquals(0, p.getRed());
        assertEquals(0, p.getGreen());
        assertEquals(0, p.getBlue());
    }
    @Test
    public void arrayConstrCriticalVals() {
    	int[] h={-1,2,333};
        Pixel p = new Pixel(h);
        
        assertEquals(0, p.getRed());
        assertEquals(2, p.getGreen());
        assertEquals(255, p.getBlue());
    }
    @Test
    public void shortArrays() {
    	int[] h={};int[] h1={1,2};
        Pixel p = new Pixel(h);
        Pixel p1 = new Pixel (h1);
        assertEquals(0, p.getRed());
        assertEquals(0, p.getGreen());
        assertEquals(0, p.getBlue());
        assertEquals(1, p1.getRed());
        assertEquals(2, p1.getGreen());
        assertEquals(0, p1.getBlue());
    }
    @Test
    public void longArrays() {
    	int[] h={1,2,3,4,5};
        Pixel p = new Pixel(h);
        assertEquals(1, p.getRed());
        assertEquals(2, p.getGreen());
        assertEquals(3, p.getBlue());
        
    }
    
}
