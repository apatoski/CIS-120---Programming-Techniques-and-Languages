import java.util.Set;
import java.util.TreeSet;

/**
 * Represents a command string sent from a client to the server, after it has
 * been parsed into a more convenient form. The {@code Command} abstract class
 * has a concrete subclass corresponding to each of the possible commands that
 * can be issued by a client. The protocol specification contains more
 * information about the expected behavior of various commands.
 */
public abstract class Command {

	// The keyword protected is used so only subclasses have access to it
	protected int senderId;
	protected String sender;

	protected Command(int senderId, String sender) {
		this.senderId = senderId;
		this.sender = sender;
	}

	/**
	 * Returns the user ID of the client who issued the {@code Command}.
	 * 
	 * @return a int which is the current user ID of the client
	 */
	public int getSenderId() {
		return senderId;
	}

	/**
	 * Returns the nickname of the client who issued the {@code Command}.
	 * 
	 * @return a non-null string which is the current nickname of the client
	 */
	public String getSender() {
		return sender;
	}

	/**
	 * Processes the command and updates the server model accordingly.
	 * 
	 * @param model
	 *            An instance of the {@link ServerModelApi} class which
	 *            represents the current state of the server.
	 * @return A {@link Broadcast} object, informing clients about changes
	 *         resulting from the command.
	 */
	public abstract Broadcast updateServerModel(ServerModel model);

	/**
	 * Returns {@code true} if two {@code Command}s are equal; that is, they
	 * produce the same string representation.
	 * 
	 * @param o
	 *            the object to compare with {@code this} for equality
	 * @return true iff both objects are non-null and equal to each other
	 */
	@Override
	public boolean equals(Object o) {
		if (o == this) {
			return true;
		}
		if (!(o instanceof Command)) {
			return false;
		}
		return this.toString().equals(o.toString());
	}
}

// ==============================================================================
// Command subclasses
// ==============================================================================

/**
 * Represents a {@link Command} issued by a client to change their nickname.
 */
class NicknameCommand extends Command {
	private String newNickname;

	public NicknameCommand(int senderId, String sender, String newNickname) {
		super(senderId, sender);
		this.newNickname = newNickname;
	}

	@Override
	public Broadcast updateServerModel(ServerModel model) {

		// helper string sets import the data from model
		Set<String> helper1 = new TreeSet<String>();
		for (String x : model.byID(senderId).getChannels()) {
			helper1.addAll(model.getUsers(x));
		}
		Set<String> helper2 = new TreeSet<String>();
		helper2.addAll(model.getRegisteredUsers());

		// error case analysis
		if (!ServerModel.isValidName(newNickname))
			return Broadcast.error(this, ServerError.INVALID_NAME);
		else if (helper2.contains(newNickname))
			return Broadcast.error(this, ServerError.NAME_ALREADY_IN_USE);
		else {
			model.byID(senderId).setNick(newNickname);
			return Broadcast.okay(this, helper1);
		}
	}

	public String getNewNickname() {
		return newNickname;
	}

	@Override
	public String toString() {
		return String.format(":%s NICK %s", getSender(), newNickname);
	}
}

/**
 * Represents a {@link Command} issued by a client to create a new channel.
 */
class CreateCommand extends Command {
	private String channel;
	private boolean inviteOnly;

	public CreateCommand(int senderId, String sender, String channel, boolean inviteOnly) {
		super(senderId, sender);
		this.channel = channel;
		this.inviteOnly = inviteOnly;
	}

	@Override
	public Broadcast updateServerModel(ServerModel model) {

		Set<String> helper1 = new TreeSet<String>();
		helper1.add(sender);
		Set<String> helper2 = new TreeSet<String>();
		helper2.addAll(model.getChannels());

		if (!ServerModel.isValidName(channel))
			return Broadcast.error(this, ServerError.INVALID_NAME);
		else if (helper2.contains(channel))
			return Broadcast.error(this, ServerError.CHANNEL_ALREADY_EXISTS);
		else {
			Channel newCh = new Channel(senderId, inviteOnly);
			model.createChannel(channel, newCh);
			model.byID(senderId).addCh(channel);
			return Broadcast.okay(this, helper1);
		}
	}

	public String getChannel() {
		return channel;
	}

	public boolean isInviteOnly() {
		return inviteOnly;
	}

	@Override
	public String toString() {
		int flag = inviteOnly ? 1 : 0;
		return String.format(":%s CREATE %s %d", getSender(), channel, flag);
	}
}

/**
 * Represents a {@link Command} issued by a client to join an existing channel.
 */
class JoinCommand extends Command {
	private String channel;

	public JoinCommand(int senderId, String sender, String channel) {
		super(senderId, sender);
		this.channel = channel;
	}

	@Override
	public Broadcast updateServerModel(ServerModel model) {

		Set<String> helper1 = new TreeSet<String>();
		helper1 = (Set<String>) model.getUsers(channel);
		helper1.add(sender);
		Set<String> helper2 = new TreeSet<String>();
		helper2.addAll(model.getChannels());

		if (!helper2.contains(channel))
			return Broadcast.error(this, ServerError.NO_SUCH_CHANNEL);
		else if (model.byName(channel).getPrivacy())
			return Broadcast.error(this, ServerError.JOIN_PRIVATE_CHANNEL);
		else {

			model.byID(senderId).addCh(channel);

			return Broadcast.names(this, helper1, model.getOwner(channel));
		}

	}

	public String getChannel() {
		return channel;
	}

	@Override
	public String toString() {
		return String.format(":%s JOIN %s", getSender(), channel);
	}
}

/**
 * Represents a {@link Command} issued by a client to send a message to all
 * other clients in the channel.
 */
class MessageCommand extends Command {
	private String channel;
	private String message;

	public MessageCommand(int senderId, String sender, String channel, String message) {
		super(senderId, sender);
		this.channel = channel;
		this.message = message;
	}

	@Override
	public Broadcast updateServerModel(ServerModel model) {
        
		if (!model.getChannels().contains(channel))
			return Broadcast.error(this, ServerError.NO_SUCH_CHANNEL);
		else if (!model.getUsers(channel).contains(sender))
			return Broadcast.error(this, ServerError.USER_NOT_IN_CHANNEL);
		
		else {
			return Broadcast.okay(this, (Set<String>) model.getUsers(channel));
		}

	}

	@Override
	public String toString() {
		return String.format(":%s MESG %s :%s", getSender(), channel, message);
	}
}

/**
 * Represents a {@link Command} issued by a client to leave a channel.
 */
class LeaveCommand extends Command {
	private String channel;

	public LeaveCommand(int senderId, String sender, String channel) {
		super(senderId, sender);
		this.channel = channel;
	}

	@Override
	public Broadcast updateServerModel(ServerModel model) {
		
		if (!model.getChannels().contains(channel))
			return Broadcast.error(this, ServerError.NO_SUCH_CHANNEL);
		else if (!model.getUsers(channel).contains(sender))
			return Broadcast.error(this, ServerError.USER_NOT_IN_CHANNEL);
		
		else {
			Set<String> helper = new TreeSet<String>();
			helper.addAll(model.getUsers(channel));

			// if the admin kicked himself delete channel
			if (model.getOwner(channel).equals(sender)) {
				for (String user : model.getUsers(channel)) {
					model.byNick(user).removeCh(channel);
				}
				model.deleteChannel(channel);
				return Broadcast.okay(this, helper);

			} else {
				model.byID(senderId).removeCh(channel);
				return Broadcast.okay(this, helper);
			}

		}

	}

	@Override
	public String toString() {
		return String.format(":%s LEAVE %s", getSender(), channel);
	}
}

/**
 * Represents a {@link Command} issued by a client to add another client to an
 * invite-only channel owned by the sender.
 */
class InviteCommand extends Command {
	private String channel;
	private String userToInvite;

	public InviteCommand(int senderId, String sender, String channel, String userToInvite) {
		super(senderId, sender);
		this.channel = channel;
		this.userToInvite = userToInvite;
	}

	@Override
	public Broadcast updateServerModel(ServerModel model) {

		Set<String> helper1 = new TreeSet<String>();
		helper1.addAll(model.getRegisteredUsers());
		Set<String> helper2 = new TreeSet<String>();
		helper2.addAll(model.getChannels());
		Set<String> helper3 = new TreeSet<String>();
		helper3.addAll(model.getUsers(channel));
		helper3.add(userToInvite);

		if (!helper1.contains(userToInvite))
			return Broadcast.error(this, ServerError.NO_SUCH_USER);
		else if (!helper2.contains(channel))
			return Broadcast.error(this, ServerError.NO_SUCH_CHANNEL);
		else if (!model.byName(channel).getPrivacy())
			return Broadcast.error(this, ServerError.INVITE_TO_PUBLIC_CHANNEL);
		else if (model.byName(channel).getOwnerID() != senderId)
			return Broadcast.error(this, ServerError.USER_NOT_OWNER);
		else {
			model.byNick(userToInvite).addCh(channel);
			return Broadcast.names(this, helper3, model.getOwner(channel));
		}

	}

	public String getChannel() {
		return channel;
	}

	public String getUserToInvite() {
		return userToInvite;
	}

	@Override
	public String toString() {
		return String.format(":%s INVITE %s %s", getSender(), channel, userToInvite);
	}
}

/**
 * Represents a {@link Command} issued by a client to remove another client from
 * an invite-only channel owned by the sender.
 */
class KickCommand extends Command {
	private String channel;
	private String userToKick;

	public KickCommand(int senderId, String sender, String channel, String userToKick) {
		super(senderId, sender);
		this.channel = channel;
		this.userToKick = userToKick;
	}

	@Override
	public Broadcast updateServerModel(ServerModel model) {
		Set<String> helper1 = new TreeSet<String>();
		helper1.addAll(model.getRegisteredUsers());
		Set<String> helper2 = new TreeSet<String>();
		helper2.addAll(model.getChannels());
		Set<String> helper3 = new TreeSet<String>();
		helper3.addAll(model.getUsers(channel));

		if (!helper1.contains(userToKick))
			return Broadcast.error(this, ServerError.NO_SUCH_USER);
		else if (!helper2.contains(channel))
			return Broadcast.error(this, ServerError.NO_SUCH_CHANNEL);
		else if (model.byName(channel).getOwnerID() != senderId)
			return Broadcast.error(this, ServerError.USER_NOT_OWNER);
		else if (!helper3.contains(userToKick))
			return Broadcast.error(this, ServerError.USER_NOT_IN_CHANNEL);
		
		
		else if (userToKick == sender) {
			helper3.add(userToKick);
			for (String user : model.getUsers(channel)) {
				model.byNick(user).removeCh(channel);
			}
			model.deleteChannel(channel);
			return Broadcast.okay(this, helper3);
		} 
		else {
			helper3.add(userToKick);
			model.byNick(userToKick).removeCh(channel);
			return Broadcast.okay(this, helper3);
		}
	}

	@Override
	public String toString() {
		return String.format(":%s KICK %s %s", getSender(), channel, userToKick);
	}
}
