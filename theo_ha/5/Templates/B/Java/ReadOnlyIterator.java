import java.util.Iterator;

public class ReadOnlyIterator<T> implements Iterator<T> {

	private final Iterator<T> it;
	
	public ReadOnlyIterator(Iterator<T> it) {
		this.it = it;
	}
	
	@Override
	public boolean hasNext() {
		return it.hasNext();
	}

	@Override
	public T next() {
		return it.next();
	}
	
}
