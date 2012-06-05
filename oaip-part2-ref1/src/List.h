#ifndef _RG_Uni_DataStructures_List_h
#define _RG_Uni_DataStructures_List_h

#include <cstddef>
#include <stdio.h>

namespace RG {
namespace Uni {
namespace DataStructures {

	template <typename El_t>
	class ListNode {
	public:
		ListNode(El_t value);
		virtual ~ListNode();

		const El_t Value;
		ListNode<El_t>* Prev;
		ListNode<El_t>* Next;
	};

	template <typename El_t>
	class List {
	private:
		List( const List<El_t>& other ) {}
		List<El_t>& operator=( const List<El_t>& other ) {}
		int _count;
	public:
		List();
		virtual ~List();

		ListNode<El_t>* Head;
		ListNode<El_t>* Tail;

		bool is_empty() const;
		int count() const;

		List<El_t>* push_back( const El_t& elt );
		List<El_t>* push_front( const El_t& elt );

		El_t pop_front();
		El_t pop_back();

		int to_buffer( El_t* buffer );
	};


	// Implementation

	template <typename El_t>
	ListNode<El_t>::ListNode(El_t value) : Value(value), Next(NULL), Prev(NULL) {}
	
	template <typename El_t>
	ListNode<El_t>::~ListNode() {}

	template <typename El_t>
	List<El_t>::List() : _count(0), Head(NULL), Tail(NULL) {}

	template <typename El_t>
	List<El_t>::~List() {
		while ( !is_empty() ) {
			pop_front();
		}
	}

	template <typename El_t>
	List<El_t>* List<El_t>::push_back( const El_t& elt ) {
		ListNode<El_t>* node = new ListNode<El_t>( elt );
		if ( Head == NULL ) {
			Head = node;
			Tail = node;
		}
		else {
			Tail->Next = node;
			node->Prev = Tail;

			Tail = node;
		}
		_count++;
	}

	template <typename El_t>
	List<El_t>* List<El_t>::push_front( const El_t& elt ) {
		ListNode<El_t>* node = new ListNode<El_t>( elt );
		if ( Head == NULL ) {
			Head = node;
			Tail = node;
		}
		else {
			Head->Prev = node;
			node->Next = Head;

			Head = node;
		}
		_count++;
	}

	template <typename El_t>
	bool List<El_t>::is_empty() const {
		return ( _count == 0 );
	}
	template <typename El_t>
	int List<El_t>::count() const {
		return _count;
	}

	template <typename El_t>
	El_t List<El_t>::pop_front() {
		if ( Head == NULL ) { throw "list is empty"; }
		
		ListNode<El_t>* node = NULL;
		if ( Head == Tail ) {
			node = Head;
			Head = NULL;
			Tail = NULL;
		}
		else {
			node = Head;
			Head = Head->Next;
		}

		_count--;
		
		El_t val = node->Value;
		delete node;
		return val;
	}

	template <typename El_t>
	El_t List<El_t>::pop_back() {
		if ( Head == NULL ) { throw "list is empty"; }
		
		ListNode<El_t>* node = NULL;
		if ( Head == Tail ) {
			node = Head;
			Head = NULL;
			Tail = NULL;
		}
		else {
			node = Tail;
			Tail = Tail->Prev;
		}

		_count--;
		
		El_t val = node->Value;
		delete node;
		return val;
	}

	template <typename El_t>
	int List<El_t>::to_buffer( El_t* buffer ) {
		int pos = 0;
		while ( ! is_empty() ) {
			buffer[pos] = pop_front();
			pos++;
		}
		return pos;
	}
}}}

#endif // _RG_Uni_DataStructures_List_h
