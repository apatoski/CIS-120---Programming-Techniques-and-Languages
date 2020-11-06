import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.Arrays;
import java.util.Collections;
import java.util.Map;
import java.util.TreeMap;

public class IO {

	BufferedReader in;
	BufferedWriter out=null;
	
	
	
	
	public IO() {
		try {
			in = new BufferedReader(new FileReader("scores.meow"));
		} catch (FileNotFoundException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		  
	}
	

	Map<Integer,String> loadEntries(){
		Map<Integer,String> entries = new TreeMap<Integer,String>(Collections.reverseOrder());
	
		 
		try {
			while(in.ready()){
				String todo=in.readLine();
				String user = todo.substring(1, todo.indexOf(','));
				Integer score=Integer.valueOf(todo.substring(todo.indexOf(',') + 1,todo.length()-1));
				entries.put(score, user);
				
			}
			in.close();
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
			return entries;
		}
		return entries;
		
	}
	public void saveEntry(Integer score,String username)
	{
		Map<Integer,String> entries = loadEntries();
		
		try {
			out = new BufferedWriter (new FileWriter("scores.meow",false));
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	 
	 
		if(!entries.containsKey(score)){
		entries.put(score, username);
		}
		else {
		String helper = entries.get(score);
		helper+= " & " + username;
		entries.remove(score);
		entries.put(score, helper);
		}
		
		
		for(Map.Entry<Integer,String> ee :entries.entrySet()){
			try {
				out.write("["+ee.getValue()+","+ee.getKey()+"]\n");
		
			} catch (IOException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			};
			
			try {
				out.flush();
			} catch (IOException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
			
		}
		        
			}
		
		

   
	
    
}
