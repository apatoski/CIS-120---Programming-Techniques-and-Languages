import org.junit.Before;
import org.junit.Test;
import static org.junit.Assert.*;

import java.util.Collection;

public class ServerModelTest {
	private ServerModel model;

	@Before
	public void setUp() {
		// We initialize a fresh ServerModel for each test
		model = new ServerModel();
	}

	@Test
    public void deregisterAdmin() {
		model.registerUser(0);
		model.byID(0).setNick("admin");
		Channel x= new Channel (0,false);
		model.createChannel("pub", x);
		model.deregisterUser(0);
        assertFalse("channel is deleted", model.getChannels().contains("pub"));
    }
	@Test
	public void invalidChannelName() {
		model.registerUser(0);
	    Command command = new CreateCommand(0, "User0", "@#%@$f2c2ecx2s23__00 ef",true);
        command.updateServerModel(model);
        assertTrue("channel is not created", model.getChannels().isEmpty());
    }
	@Test
	public void adminAutoKick() {
		model.registerUser(0);
	    Command command = new CreateCommand(0, "User0", "lol",true);
        command.updateServerModel(model);
        Command kick = new KickCommand(0, "User0" ,"lol", "User0" );
        kick.updateServerModel(model);
        assertTrue("channel is not created", model.getChannels().isEmpty());
    }
	@Test
	public void noChan() {
		model.registerUser(0);
	    Command command = new CreateCommand(0, "User0", "lol",true);
        command.updateServerModel(model);
        Command kick = new KickCommand(0, "User0" ,"lo", "User0" );
        
        assertEquals(kick.updateServerModel(model),Broadcast.error(kick, ServerError.NO_SUCH_CHANNEL));
    }
	@Test
	public void noUser() {
		model.registerUser(0);
	    Command command = new CreateCommand(0, "User0", "lol",true);
        command.updateServerModel(model);
        Command kick = new KickCommand(0, "User0" ,"lol", "User1" );
        assertEquals(kick.updateServerModel(model),Broadcast.error(kick, ServerError.NO_SUCH_USER));
    }
	@Test
	public void userNotIn() {
		model.registerUser(0);
		model.registerUser(1);
	    Command command = new CreateCommand(0, "User0", "lol",true);
        command.updateServerModel(model);
        Command kick = new KickCommand(0, "User0" ,"lol", "User1" );
        assertEquals(kick.updateServerModel(model),Broadcast.error(kick, ServerError.USER_NOT_IN_CHANNEL));
    }
	
	
	
	
	
	
}
