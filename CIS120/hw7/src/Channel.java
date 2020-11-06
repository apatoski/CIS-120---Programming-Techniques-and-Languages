
public class Channel {
	private Integer ownerID;
	private Boolean inviteOnly;

	Channel(Integer _id, Boolean _inv) {
		ownerID = _id;
		inviteOnly = _inv;
	}

	public Integer getOwnerID() {
		return ownerID;
	}

	public boolean getPrivacy() {
		return inviteOnly;
	}

	public void setPrivacy(Boolean flag) {
		inviteOnly = flag;
	}
	// I want equals to remain referential
}
