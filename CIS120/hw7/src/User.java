
import java.util.Set;
import java.util.TreeSet;

public class User {
	private String nick;
	private Set<String> chList;

	User(String _nick) {
		nick = _nick;
		chList = new TreeSet<String>();
	}

	public Set<String> getChannels() {
		return chList;
	}

	public String getNick() {
		return nick;
	}

	public void setNick(String n) {
		nick = n;
	}
	
	public void addCh(String name) {
		chList.add(name);

	}

	public void removeCh(String name) {
		chList.remove(name);

	}
// I want equals to remain referential
}
