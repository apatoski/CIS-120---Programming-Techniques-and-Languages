import java.util.Collection;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;
import java.util.TreeMap;

/**
 * The {@code ServerModel} is the class responsible for tracking the state of
 * the server, including its current users and the channels they are in. This
 * class is used by subclasses of {@link Command} to handle commands from
 * clients, as well as the {@link ServerBackend} to coordinate client connection
 * and disconnection.
 */
public final class ServerModel implements ServerModelApi {

	private TreeMap<Integer, User> users;
	private TreeMap<String, Channel> channels;

	/**
	 * Constructs a {@code ServerModel} and initializes any collections needed
	 * for modeling the server state.
	 */
	public ServerModel() {
		// TODO: Initialize your state here
		users = new TreeMap<Integer, User>();
		channels = new TreeMap<String, Channel>();
	}

	// ==========================================================================
	// Client connection handlers
	// ==========================================================================

	/**
	 * Informs the model that a client has connected to the server with the
	 * given user ID. The model should update its state so that it can identify
	 * this user during later interactions with the model. Any user that is
	 * registered with the server (without being later deregistered) should
	 * appear in the output of {@link #getRegisteredUsers()}.
	 * 
	 * @param userId
	 *            the unique ID created by the backend to represent this user
	 * @return a {@link Broadcast} informing the user of their new nickname
	 */
	public Broadcast registerUser(int userId) {
		String nickname = generateUniqueNickname();
		User us = new User(nickname);
		users.put(userId, us);
		// TODO: Return broadcast upon user connection
		Broadcast c = Broadcast.connected(nickname);
		return c;
	}

	// creation and deletion of channels
	public void createChannel(String channel, Channel c) {
		channels.put(channel, c);
	}

	public void deleteChannel(String channel) {
		channels.remove(channel);
	}

	/**
	 * Generates a unique nickname of the form "UserX", where X is the smallest
	 * non-negative integer that yields a unique nickname for a user.
	 * 
	 * @return the generated nickname
	 */
	private String generateUniqueNickname() {
		int suffix = 0;
		String nickname;
		Collection<String> existingUsers = getRegisteredUsers();
		do {
			nickname = "User" + suffix++;
		} while (existingUsers != null && existingUsers.contains(nickname));
		return nickname;
	}

	/**
	 * Determines if a given nickname is valid or invalid (contains at least one
	 * alphanumeric character, and no non-alphanumeric characters).
	 * 
	 * @param name
	 *            The channel or nickname string to validate
	 * @return true if the string is a valid name
	 */
	public static boolean isValidName(String name) {
		if (name == null || name.isEmpty()) {
			return false;
		}
		for (char c : name.toCharArray()) {
			if (!Character.isLetterOrDigit(c)) {
				return false;
			}
		}
		return true;
	}

	/**
	 * Informs the model that the client with the given user ID has disconnected
	 * from the server. After a user ID is deregistered, the server backend is
	 * free to reassign this user ID to an entirely different client. As such,
	 * the model should take care to expunge any state pertaining to a user who
	 * has been deregistered. Any user that is deregistered (without later being
	 * registered) should not appear in the output of
	 * {@link #getRegisteredUsers()}. The behavior of this method if the given
	 * user ID is not registered with the model is undefined.
	 * 
	 * @param userId
	 *            the unique ID created by the backend to represent this user
	 * @return A {@link Broadcast} informing other clients in the disconnected
	 *         user's channels that they have disconnected
	 */
	public Broadcast deregisterUser(int userId) {

		String nickname = getNickname(userId);

		Set<String> helper = new TreeSet<String>();
		for (String i : byID(userId).getChannels()) {
			for (User j : users.values())
				if (j.getChannels().contains(i) && byID(userId) != j)
					helper.add(j.getNick());
		}
		for (Map.Entry <String,Channel> ch:channels.entrySet())
		{
			if (ch.getValue().getOwnerID()==userId) deleteChannel(ch.getKey());
		}
		users.remove(userId);
		Broadcast c = Broadcast.disconnected(nickname, helper);
		return c;
	}

	// ==========================================================================
	// Model update functions
	// ==========================================================================
	// TODO: Add functions that update your model

	// ==========================================================================
	// Server model queries
	// These functions provide helpful ways to test the state of your model.
	// You may also use them in your implementation.
	// ==========================================================================

	/**
	 * Returns the user ID currently associated with the given nickname. The
	 * returned ID is -1 if the nickname is not currently in use.
	 * 
	 * @param nickname
	 *            The user's nickname
	 * @return the id of said user
	 */
	public int getUserId(String nickname) {
		for (Map.Entry<Integer, User> entry : users.entrySet()) {
			if (((entry.getValue()).getNick()).equals(nickname)) {
				return entry.getKey();
			}
		}
		return -1;
	}

	/**
	 * Returns the nickname currently associated with the given user ID. The
	 * returned string is null if the user ID is not currently in use.
	 * 
	 * @param userId
	 *            The ID whose nickname to return.
	 * @return The nickname associated with the current ID.
	 */
	public String getNickname(int userId) {
		if (users.containsKey(userId))
			return byID(userId).getNick();
		return null;
	}

	/**
	 * Returns a collection of the nicknames of all users that are registered
	 * with the server. Provided for testing.
	 * 
	 * @return the collection of registered user nicknames
	 */
	public Collection<String> getRegisteredUsers() {
		// TODO: Return users connected to server
		TreeSet<String> registratedUsers = new TreeSet<String>();
		for (Map.Entry<Integer, User> entry : users.entrySet()) {
			registratedUsers.add(entry.getValue().getNick());
		}
		return registratedUsers;
	}

	/**
	 * Returns a collection of the names of all the channels that are present on
	 * the server. The returned collection is empty if no channels exist.
	 * Provided for testing.
	 * 
	 * @return the collection of channel names
	 */
	public Collection<String> getChannels() {
		TreeSet<String> runningChannels = new TreeSet<String>();
		for (Map.Entry<String, Channel> entry : channels.entrySet()) {
			runningChannels.add(entry.getKey());
		}

		return runningChannels;
	}

	/**
	 * Returns a collection of the nicknames of all the users in a given
	 * channel. The returned collection is empty if no channel with the given
	 * name exists. Provided for testing.
	 * 
	 * @param channelName
	 *            The channel whose member nicknames should be returned
	 * @return the collection of user nicknames in the current channel
	 */
	public Collection<String> getUsers(String channelName) {
		TreeSet<String> registredUsers = new TreeSet<String>();
		for (Map.Entry<Integer, User> entry : users.entrySet()) {
			Set<String> helper = new TreeSet<String>();
			helper.addAll(entry.getValue().getChannels());
			for (String i : helper) {
				if (i.equals(channelName))
					registredUsers.add(entry.getValue().getNick());
			}
		}
		return registredUsers;
	}

	/**
	 * e Returns the nickname of the owner of the current channel. The result is
	 * {@code null} if no channel with the given name exists. Provided for
	 * testing.
	 * 
	 * @param channelName
	 *            The channel whose owner nickname should be returned
	 * @return the nickname of the channel owner
	 */

	public String getOwner(String channelName) {
		// TODO: Return owner of the channel

		return users.get(byName(channelName).getOwnerID()).getNick();
	}

	public User byID(Integer id) {
		return users.get(id);
	}

	public User byNick(String nick) {
		return users.get(getUserId(nick));
	}

	public Channel byName(String channel) {
		return channels.get(channel);
	}

}
